------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 E R R O U T C . P R E T T Y _ E M I T T E R              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Namet;      use Namet;
with Opt;        use Opt;
with Output;     use Output;
with Sinput;     use Sinput;
with GNAT.Lists; use GNAT.Lists;

package body Erroutc.Pretty_Emitter is

   REGION_OFFSET : constant := 1;
   --  Number of characters between the line bar and the region span

   REGION_ARM_SIZE : constant := 2;
   --  Number of characters on the region span arms
   --  e.g. two for this case:
   --   +--
   --   |
   --   +--
   --   ^^

   REGION_SIZE : constant := REGION_OFFSET + 1 + REGION_ARM_SIZE;
   --  The total number of characters taken up by the region span characters

   MAX_BAR_POS : constant := 7;
   --  The maximum position of the line bar from the start of the line

   procedure Destroy (Elem : in out Labeled_Span_Type);
   pragma Inline (Destroy);

   procedure Destroy (Elem : in out Labeled_Span_Type) is
   begin
      --  Diagnostic elements will be freed when all the diagnostics have been
      --  emitted.
      null;
   end Destroy;

   package Labeled_Span_Lists is new Doubly_Linked_Lists
     (Element_Type    => Labeled_Span_Type,
      "="             => "=",
      Destroy_Element => Destroy,
      Check_Tampering => False);
   subtype Labeled_Span_List is Labeled_Span_Lists.Doubly_Linked_List;

   type Printable_Line is record
      First : Source_Ptr;
      --  The first character of the line

      Last : Source_Ptr;
      --  The last character of the line

      Line_Nr : Pos;
      --  The line number

      Spans : Labeled_Span_List;
      --  The spans applied on the line
   end record;

   procedure Destroy (Elem : in out Printable_Line);
   pragma Inline (Destroy);

   function Equals (L, R : Printable_Line) return Boolean is
     (L.Line_Nr = R.Line_Nr);

   package Lines_Lists is new Doubly_Linked_Lists
     (Element_Type    => Printable_Line,
      "="             => Equals,
      Destroy_Element => Destroy,
      Check_Tampering => False);

   subtype Lines_List is Lines_Lists.Doubly_Linked_List;

   type File_Sections is record
      File : String_Ptr;
      --  Name of the file

      Ptr : Source_Ptr;
      --  Pointer to the Primary location in the file section that is printed
      --  at the start of the file section. If there are none then the first
      --  location in the section.

      Lines : Lines_List;
      --  Lines to be printed for the file
   end record;

   procedure Destroy (Elem : in out File_Sections);
   pragma Inline (Destroy);

   function Equals (L, R : File_Sections) return Boolean is
     (L.File /= null and then R.File /= null and then L.File.all = R.File.all);

   package File_Section_Lists is new Doubly_Linked_Lists
     (Element_Type    => File_Sections,
      "="             => Equals,
      Destroy_Element => Destroy,
      Check_Tampering => False);

   subtype File_Section_List is File_Section_Lists.Doubly_Linked_List;

   function Create_File_Sections
     (Locations : Labeled_Span_Id) return File_Section_List;
   --  Create a list of file sections from the labeled spans that are to be
   --  printed.
   --
   --  Each file section contains a list of lines that are to be printed for
   --  the file and the spans that are applied to each of those lines.

   procedure Create_File_Section
     (Sections : in out File_Section_List; Loc : Labeled_Span_Type);
   --  Create a new file section for the given labeled span.

   procedure Add_Printable_Line
     (Lines : Lines_List; Loc : Labeled_Span_Type; S_Ptr : Source_Ptr);

   procedure Create_Printable_Line
     (Lines : Lines_List; Loc : Labeled_Span_Type; S_Ptr : Source_Ptr);
   --  Create a new printable line for the given labeled span and add it in the
   --  correct position to the Lines list based on the line number.

   function Get_Region_Span
     (Spans : Labeled_Span_List) return Labeled_Span_Type;

   function Has_Multiple_Labeled_Spans (L : Printable_Line) return Boolean;

   procedure Write_Region_Delimiter (SGR_Code : String);
   --  Write the arms signifying the start and end of a region span
   --  e.g. +--

   procedure Write_Region_Bar (SGR_Code : String);
   --  Write the bar signifying the continuation of a region span
   --  e.g. |

   procedure Write_Region_Continuation (SGR_Code : String);
   --  Write the continuation signifying the continuation of a region span
   --  e.g. :

   procedure Write_Region_Offset;
   --  Write a number of whitespaces equal to the size of the region span

   function Trimmed_Image (I : Natural) return String;
   --  Removes the leading whitespace from the 'Image of a Natural number.

   procedure Write_Span_Labels
     (Loc                  : Labeled_Span_Type;
      L                    : Printable_Line;
      Line_Size            : Integer;
      Idx                  : String;
      Within_Region_Span   : Boolean;
      SGR_Code             : String;
      Region_Span_SGR_Code : String);

   procedure Write_File_Section
     (Sec              : File_Sections;
      Write_File_Name  : Boolean;
      File_Name_Offset : Integer;
      Include_Spans    : Boolean;
      SGR_Code         : String := SGR_Note);
   --  Prints the labled spans for a given File_Section.
   --
   --  --> <File_Section.File_Name>
   --  <Labeled_Spans inside the file>

   procedure Write_Labeled_Spans
     (Locations        : Labeled_Span_Id;
      Write_File_Name  : Boolean;
      File_Name_Offset : Integer;
      Include_Spans    : Boolean := True;
      SGR_Code         : String := SGR_Note);
   --  Pretty-prints all of the code regions indicated by the Locations. The
   --  labeled spans in the Locations are grouped by file into File_Sections
   --  and sorted by the file name of the Primary location followed by all
   --  other locations sorted alphabetically.

   procedure Write_Intersecting_Labels
     (Intersecting_Labels : Labeled_Span_List; SGR_Code : String);
   --  Prints the indices and their associated labels of intersecting labels.
   --
   --  Labeled spans that are insercting on the same line are printed without
   --  labels. Instead the span pointer is replaced by an index number and in
   --  the end all of the indices are printed with their associated labels.
   --
   --
   --  42 |  [for I in V1.First_Index .. V1.Last_Index => V1(I), -6];
   --     |   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   --     |                                                      1-
   --     |       2-------------------------------------------
   --     | 1: positional element
   --     | 2: named element

   function Get_Line_End
     (Buf : Source_Buffer_Ptr; Loc : Source_Ptr) return Source_Ptr;
   --  Get the source location for the end of the line (LF) in Buf for Loc. If
   --  Loc is past the end of Buf already, return Buf'Last.

   function Get_Line_Start
     (Buf : Source_Buffer_Ptr; Loc : Source_Ptr) return Source_Ptr;
   --  Get the source location for the start of the line in Buf for Loc

   function Get_First_Line_Char
     (Buf : Source_Buffer_Ptr; Loc : Source_Ptr) return Source_Ptr;
   --  Get first non-space character in the line containing Loc

   function Get_Last_Line_Char
     (Buf : Source_Buffer_Ptr; Loc : Source_Ptr) return Source_Ptr;
   --  Get last non line end [LF, CR] character in the line containing Loc

   function Image (X : Positive; Width : Positive) return String;
   --  Output number X over Width characters, with whitespace padding.
   --  Only output the low-order Width digits of X, if X is larger than
   --  Width digits.

   procedure Write_Buffer
     (Buf : Source_Buffer_Ptr; First : Source_Ptr; Last : Source_Ptr);
   --  Output the characters from First to Last position in Buf, using
   --  Write_Buffer_Char.

   procedure Write_Buffer_Char (Buf : Source_Buffer_Ptr; Loc : Source_Ptr);
   --  Output the characters at position Loc in Buf, translating ASCII.HT
   --  in a suitable number of spaces so that the output is not modified
   --  by starting in a different column that 1.

   procedure Write_Line_Marker (Num : Pos; Width : Positive);
   --  Attempts to write the line number within Width number of whitespaces
   --  followed by a bar ':' symbol.
   --
   --  e.g '  12 |'
   --
   --  This is usually used on source code lines that are marked by a span.

   procedure Write_Empty_Bar_Line (Width : Integer);
   --  Writes Width number of whitespaces and a bar '|' symbol.
   --
   --  e.g '     |'
   --
   --  This is usually used on lines where the label is going to printed.

   procedure Write_Empty_Skip_Line (Width : Integer);
   --  Writes Width number of whitespaces and a bar ':' symbol.
   --
   --  e.g '     :'
   --
   --  This is usually used between non-continous source lines that neec to be
   --  printed.

   procedure Write_Error_Msg_Line (E_Msg : Error_Msg_Object);
   --  Write the error message line for the given diagnostic:
   --
   --  '['<Diag.Id>']' <Diag.Kind>: <Diag.Message> ['['<Diag.Switch>']']

   function Should_Write_File_Name
     (Sub_Diag : Error_Msg_Object; Diag : Error_Msg_Object) return Boolean;
   --  If the sub-diagnostic and the main diagnostic only point to the same
   --  file then there is no reason to add the file name to the sub-diagnostic.

   function Should_Write_Spans
     (Sub_Diag : Error_Msg_Object; Diag : Error_Msg_Object) return Boolean;
   --  Old sub-diagnostics used to have the same location as the main
   --  diagnostic in order to group them correctly. However in most cases
   --  it was not meant to point to a location but rather add an additional
   --  message to the original diagnostic.
   --
   --  If the sub-diagnostic and the main diagnostic have the same location
   --  then we should avoid printing the spans.

   procedure Print_Diagnostic (E : Error_Msg_Id);
   --  Entry point for printing a primary diagnostic message.

   procedure Print_Edit (Edit : Edit_Type; Offset : Integer);
   --  Prints an edit object as follows:
   --
   --  --> <File_Name>
   --  -<Line_Nr> <Old_Line>
   --  +<Line_Nr> <New_Line>

   procedure Print_Fix (Fix : Fix_Type; Offset : Integer);
   --  Prints a fix object as follows
   --
   --  + Fix: <Fix.Description>
   --  <Fix.Edits>

   procedure Print_Sub_Diagnostic
     (Sub_Diag : Error_Msg_Object; Diag : Error_Msg_Object; Offset : Integer);

   function To_String (Sptr : Source_Ptr) return String;
   --  Convert the source pointer to a string of the form: "file:line:column"

   function To_File_Name (Sptr : Source_Ptr) return String;
   --  Converts the file name of the Sptr to a string.

   function Line_To_String (Sptr : Source_Ptr) return String;
   --  Converts the logical line number of the Sptr to a string.

   function Column_To_String (Sptr : Source_Ptr) return String;
   --  Converts the column number of the Sptr to a string. Column values less
   --  than 10 are prefixed with a 0.

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Elem : in out Printable_Line) is
   begin
      Labeled_Span_Lists.Destroy (Elem.Spans);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Elem : in out File_Sections) is
   begin
      Free (Elem.File);
      Lines_Lists.Destroy (Elem.Lines);
   end Destroy;

   ------------------
   -- Get_Line_End --
   ------------------

   function Get_Line_End
     (Buf : Source_Buffer_Ptr; Loc : Source_Ptr) return Source_Ptr
   is
      Cur_Loc : Source_Ptr := Source_Ptr'Min (Loc, Buf'Last);
   begin
      while Cur_Loc < Buf'Last and then Buf (Cur_Loc) /= ASCII.LF loop
         Cur_Loc := Cur_Loc + 1;
      end loop;

      return Cur_Loc;
   end Get_Line_End;

   --------------------
   -- Get_Line_Start --
   --------------------

   function Get_Line_Start
     (Buf : Source_Buffer_Ptr; Loc : Source_Ptr) return Source_Ptr
   is
      Cur_Loc : Source_Ptr := Loc;
   begin
      while Cur_Loc > Buf'First and then Buf (Cur_Loc - 1) /= ASCII.LF loop
         Cur_Loc := Cur_Loc - 1;
      end loop;

      return Cur_Loc;
   end Get_Line_Start;

   -------------------------
   -- Get_First_Line_Char --
   -------------------------

   function Get_First_Line_Char
     (Buf : Source_Buffer_Ptr; Loc : Source_Ptr) return Source_Ptr
   is
      Cur_Loc : Source_Ptr := Get_Line_Start (Buf, Loc);
   begin
      while Cur_Loc < Buf'Last and then Buf (Cur_Loc) = ' ' loop
         Cur_Loc := Cur_Loc + 1;
      end loop;

      return Cur_Loc;
   end Get_First_Line_Char;

   ------------------------
   -- Get_Last_Line_Char --
   ------------------------

   function Get_Last_Line_Char
     (Buf : Source_Buffer_Ptr; Loc : Source_Ptr) return Source_Ptr
   is
      Cur_Loc : Source_Ptr := Get_Line_End (Buf, Loc);
   begin
      while Cur_Loc > Buf'First
        and then Buf (Cur_Loc) in ASCII.LF | ASCII.CR
      loop
         Cur_Loc := Cur_Loc - 1;
      end loop;

      return Cur_Loc;
   end Get_Last_Line_Char;

   -----------
   -- Image --
   -----------

   function Image (X : Positive; Width : Positive) return String is
      Str  : String (1 .. Width);
      Curr : Natural := X;
   begin
      for J in reverse 1 .. Width loop
         if Curr > 0 then
            Str (J) := Character'Val (Character'Pos ('0') + Curr mod 10);
            Curr    := Curr / 10;
         else
            Str (J) := ' ';
         end if;
      end loop;

      return Str;
   end Image;

   --------------------------------
   -- Has_Multiple_Labeled_Spans --
   --------------------------------

   function Has_Multiple_Labeled_Spans (L : Printable_Line) return Boolean is
      Count : Natural := 0;

      Loc    : Labeled_Span_Type;
      Loc_It : Labeled_Span_Lists.Iterator :=
        Labeled_Span_Lists.Iterate (L.Spans);
   begin
      while Labeled_Span_Lists.Has_Next (Loc_It) loop
         Labeled_Span_Lists.Next (Loc_It, Loc);
         if Loc.Label /= null then
            Count := Count + 1;
         end if;
      end loop;

      return Count > 1;
   end Has_Multiple_Labeled_Spans;

   ---------------------
   -- Get_Region_Span --
   ---------------------

   function Get_Region_Span
     (Spans : Labeled_Span_List) return Labeled_Span_Type
   is
      Loc    : Labeled_Span_Type;
      Loc_It : Labeled_Span_Lists.Iterator :=
        Labeled_Span_Lists.Iterate (Spans);
   begin
      while Labeled_Span_Lists.Has_Next (Loc_It) loop
         Labeled_Span_Lists.Next (Loc_It, Loc);

         if Loc.Is_Region then
            return Loc;
         end if;
      end loop;

      return No_Labeled_Span_Object;
   end Get_Region_Span;

   ------------------
   -- Write_Buffer --
   ------------------

   procedure Write_Buffer
     (Buf : Source_Buffer_Ptr; First : Source_Ptr; Last : Source_Ptr)
   is
   begin
      for Loc in First .. Last loop
         Write_Buffer_Char (Buf, Loc);
      end loop;
   end Write_Buffer;

   -----------------------
   -- Write_Buffer_Char --
   -----------------------

   procedure Write_Buffer_Char (Buf : Source_Buffer_Ptr; Loc : Source_Ptr) is
   begin
      --  If the character ASCII.HT is not the last one in the file,
      --  output as many spaces as the character represents in the
      --  original source file.

      if Buf (Loc) = ASCII.HT and then Loc < Buf'Last then
         for X in Get_Column_Number (Loc) .. Get_Column_Number (Loc + 1) - 1
         loop
            Write_Char (' ');
         end loop;

      --  Otherwise output the character itself

      else
         Write_Char (Buf (Loc));
      end if;
   end Write_Buffer_Char;

   -----------------------
   -- Write_Line_Marker --
   -----------------------

   procedure Write_Line_Marker (Num : Pos; Width : Positive) is
   begin
      Write_Str (Image (Positive (Num), Width => Width - 2));
      Write_Str (" |");
   end Write_Line_Marker;

   --------------------------
   -- Write_Empty_Bar_Line --
   --------------------------

   procedure Write_Empty_Bar_Line (Width : Integer) is

   begin
      Write_Str (String'(1 .. Width - 1 => ' '));
      Write_Str ("|");
   end Write_Empty_Bar_Line;

   ---------------------------
   -- Write_Empty_Skip_Line --
   ---------------------------

   procedure Write_Empty_Skip_Line (Width : Integer) is

   begin
      Write_Str (String'(1 .. Width - 1 => ' '));
      Write_Str (":");
   end Write_Empty_Skip_Line;

   ----------------------------
   -- Write_Region_Delimiter --
   ----------------------------

   procedure Write_Region_Delimiter (SGR_Code : String) is

   begin
      Write_Str (String'(1 .. REGION_OFFSET => ' '));
      Write_Str (SGR_Code);
      Write_Str ("+");
      Write_Str (String'(1 .. REGION_ARM_SIZE => '-'));
      Write_Str (SGR_Reset);
   end Write_Region_Delimiter;

   ----------------------
   -- Write_Region_Bar --
   ----------------------

   procedure Write_Region_Bar (SGR_Code : String) is

   begin
      Write_Str (String'(1 .. REGION_OFFSET => ' '));
      Write_Str (SGR_Code);
      Write_Str ("|");
      Write_Str (SGR_Reset);
      Write_Str (String'(1 .. REGION_ARM_SIZE => ' '));
   end Write_Region_Bar;

   -------------------------------
   -- Write_Region_Continuation --
   -------------------------------

   procedure Write_Region_Continuation (SGR_Code : String) is

   begin
      Write_Str (String'(1 .. REGION_OFFSET => ' '));
      Write_Str (SGR_Code);
      Write_Str (":");
      Write_Str (SGR_Reset);
      Write_Str (String'(1 .. REGION_ARM_SIZE => ' '));
   end Write_Region_Continuation;

   -------------------------
   -- Write_Region_Offset --
   -------------------------

   procedure Write_Region_Offset is

   begin
      Write_Str (String'(1 .. REGION_SIZE => ' '));
   end Write_Region_Offset;

   ------------------------
   -- Add_Printable_Line --
   ------------------------

   procedure Add_Printable_Line
     (Lines : Lines_List;
      Loc   : Labeled_Span_Type;
      S_Ptr : Source_Ptr)
   is
      L    : Printable_Line;
      L_It : Lines_Lists.Iterator;

      Line_Ptr   : constant Pos := Pos (Get_Physical_Line_Number (S_Ptr));
      Line_Found : Boolean      := False;
   begin
      L_It := Lines_Lists.Iterate (Lines);
      while Lines_Lists.Has_Next (L_It) loop
         Lines_Lists.Next (L_It, L);

         if not Line_Found and then L.Line_Nr = Line_Ptr then
            if not Labeled_Span_Lists.Contains (L.Spans, Loc) then
               Labeled_Span_Lists.Append (L.Spans, Loc);
            end if;
            Line_Found := True;
         end if;
      end loop;

      if not Line_Found then
         Create_Printable_Line (Lines, Loc, S_Ptr);
      end if;
   end Add_Printable_Line;

   ---------------------------
   -- Create_Printable_Line --
   ---------------------------

   procedure Create_Printable_Line
     (Lines : Lines_List; Loc : Labeled_Span_Type; S_Ptr : Source_Ptr)
   is
      Spans : constant Labeled_Span_List := Labeled_Span_Lists.Create;

      Buf : constant Source_Buffer_Ptr :=
        Source_Text (Get_Source_File_Index (S_Ptr));

      Line_Nr : constant Pos := Pos (Get_Physical_Line_Number (S_Ptr));

      New_Line : constant Printable_Line :=
        (First   => Get_Line_Start (Buf, S_Ptr),
         Last    => Get_Line_End (Buf, S_Ptr),
         Line_Nr => Line_Nr,
         Spans   => Spans);

      L    : Printable_Line;
      L_It : Lines_Lists.Iterator := Lines_Lists.Iterate (Lines);

      Found_Greater_Line : Boolean := False;
      Insert_Before_Line : Printable_Line;
   begin
      Labeled_Span_Lists.Append (Spans, Loc);

      --  Insert the new line based on the line number

      while Lines_Lists.Has_Next (L_It) loop
         Lines_Lists.Next (L_It, L);

         if not Found_Greater_Line and then L.Line_Nr > New_Line.Line_Nr then
            Found_Greater_Line := True;
            Insert_Before_Line := L;

            Lines_Lists.Insert_Before (Lines, Insert_Before_Line, New_Line);
         end if;
      end loop;

      --  Insert after all the lines have been iterated over to avoid the
      --  mutation lock in GNAT.Lists.

      if not Found_Greater_Line then
         Lines_Lists.Append (Lines, New_Line);
      end if;
   end Create_Printable_Line;

   -------------------------
   -- Create_File_Section --
   -------------------------

   procedure Create_File_Section
     (Sections : in out File_Section_List; Loc : Labeled_Span_Type)
   is
      Lines : constant Lines_List := Lines_Lists.Create;

      --  Carret positions
      Ptr      : constant Source_Ptr := Loc.Span.Ptr;
      Line_Ptr : constant Pos        := Pos (Get_Physical_Line_Number (Ptr));

      --  Span start positions
      Fst      : constant Source_Ptr := Loc.Span.First;
      Line_Fst : constant Pos        := Pos (Get_Physical_Line_Number (Fst));

      --  Span end positions
      Lst      : constant Source_Ptr := Loc.Span.Last;
      Line_Lst : constant Pos        := Pos (Get_Physical_Line_Number (Lst));
   begin
      Create_Printable_Line (Lines, Loc, Fst);

      if Line_Fst /= Line_Ptr then
         Create_Printable_Line (Lines, Loc, Ptr);
      end if;

      if Line_Ptr /= Line_Lst then
         Create_Printable_Line (Lines, Loc, Lst);
      end if;

      File_Section_Lists.Append
        (Sections,
         (File  => new String'(To_File_Name (Loc.Span.Ptr)),
          Ptr   => Loc.Span.Ptr,
          Lines => Lines));
   end Create_File_Section;

   --------------------------
   -- Create_File_Sections --
   --------------------------

   function Create_File_Sections
     (Locations : Labeled_Span_Id) return File_Section_List
   is
      Loc    : Labeled_Span_Type;
      Loc_It : Labeled_Span_Id := Locations;

      Sections : File_Section_List := File_Section_Lists.Create;

      Sec  : File_Sections;
      F_It : File_Section_Lists.Iterator;

      File_Found : Boolean;
   begin
      while Loc_It /= No_Labeled_Span loop
         Loc := Erroutc.Locations.Table (Loc_It);

         File_Found := False;
         F_It       := File_Section_Lists.Iterate (Sections);

         while File_Section_Lists.Has_Next (F_It) loop
            File_Section_Lists.Next (F_It, Sec);

            if Sec.File /= null
              and then Sec.File.all = To_File_Name (Loc.Span.Ptr)
            then
               File_Found := True;

               Add_Printable_Line (Sec.Lines, Loc, Loc.Span.First);
               Add_Printable_Line (Sec.Lines, Loc, Loc.Span.Ptr);
               Add_Printable_Line (Sec.Lines, Loc, Loc.Span.Last);

               if Loc.Is_Primary then
                  Sec.Ptr := Loc.Span.Ptr;
               end if;
            end if;
         end loop;

         if not File_Found then
            Create_File_Section (Sections, Loc);
         end if;

         Loc_It := Loc.Next;
      end loop;

      return Sections;
   end Create_File_Sections;

   -----------------------
   -- Write_Span_Labels --
   -----------------------

   procedure Write_Span_Labels
     (Loc                  : Labeled_Span_Type;
      L                    : Printable_Line;
      Line_Size            : Integer;
      Idx                  : String;
      Within_Region_Span   : Boolean;
      SGR_Code             : String;
      Region_Span_SGR_Code : String)
   is
      Span_Char : constant Character := (if Loc.Is_Primary then '~' else '-');

      Buf : constant Source_Buffer_Ptr :=
        Source_Text (Get_Source_File_Index (L.First));

      Col_L_Fst : constant Natural :=
        Natural (Get_Column_Number (Get_First_Line_Char (Buf, L.First)));
      Col_L_Lst : constant Natural :=
        Natural (Get_Column_Number (Get_Last_Line_Char (Buf, L.Last)));

      --  Carret positions
      Ptr      : constant Source_Ptr := Loc.Span.Ptr;
      Line_Ptr : constant Pos        := Pos (Get_Physical_Line_Number (Ptr));
      Col_Ptr  : constant Natural    := Natural (Get_Column_Number (Ptr));

      --  Span start positions
      Fst      : constant Source_Ptr := Loc.Span.First;
      Line_Fst : constant Pos        := Pos (Get_Physical_Line_Number (Fst));
      Col_Fst  : constant Natural    := Natural (Get_Column_Number (Fst));

      --  Span end positions
      Lst      : constant Source_Ptr := Loc.Span.Last;
      Line_Lst : constant Pos        := Pos (Get_Physical_Line_Number (Lst));
      Col_Lst  : constant Natural    := Natural (Get_Column_Number (Lst));

      --  Attributes for the span on the current line

      Span_Sym : constant String := (if Idx = "" then "^" else Idx);

      Span_Fst : constant Natural :=
        (if Line_Fst = L.Line_Nr then Col_Fst else Col_L_Fst);

      Span_Lst : constant Natural :=
        (if Line_Lst = L.Line_Nr then Col_Lst else Col_L_Lst);

      Span_Ptr_Fst : constant Natural :=
        (if Line_Ptr = L.Line_Nr then Col_Ptr else Col_L_Fst);

      Span_Ptr_Lst : constant Natural :=
        (if Line_Ptr = L.Line_Nr then Span_Ptr_Fst + Span_Sym'Length
         else Span_Fst);

   begin
      if not Loc.Is_Region then
         Write_Empty_Bar_Line (Line_Size);

         if Within_Region_Span then
            Write_Region_Bar (Region_Span_SGR_Code);
         else
            Write_Region_Offset;
         end if;

         Write_Str (String'(1 .. Span_Fst - 1 => ' '));

         Write_Str (SGR_Code);

         if Line_Ptr = L.Line_Nr then
            Write_Str (String'(Span_Fst .. Col_Ptr - 1 => Span_Char));
            Write_Str (Span_Sym);
         end if;

         Write_Str (String'(Span_Ptr_Lst .. Span_Lst => Span_Char));

         Write_Str (SGR_Reset);

         Write_Eol;

         --  Write the label under the line unless it is an intersecting span.
         --  In this case omit the label which will be printed later along with
         --  the index.

         if Loc.Label /= null and then Idx = "" then
            Write_Empty_Bar_Line (Line_Size);

            if Within_Region_Span then
               Write_Region_Bar (Region_Span_SGR_Code);
            else
               Write_Region_Offset;
            end if;

            Write_Str (String'(1 .. Span_Fst - 1 => ' '));
            Write_Str (SGR_Code);
            Write_Str (Loc.Label.all);
            Write_Str (SGR_Reset);
            Write_Eol;
         end if;
      else
         if Line_Lst = L.Line_Nr then
            Write_Empty_Bar_Line (Line_Size);
            Write_Str (String'(1 .. REGION_OFFSET => ' '));
            Write_Str (SGR_Code);
            Write_Str (Loc.Label.all);
            Write_Str (SGR_Reset);
            Write_Eol;
         end if;
      end if;
   end Write_Span_Labels;

   -------------------
   -- Trimmed_Image --
   -------------------

   function Trimmed_Image (I : Natural) return String is
      Img_Raw : constant String := Natural'Image (I);
   begin
      return Img_Raw (Img_Raw'First + 1 .. Img_Raw'Last);
   end Trimmed_Image;

   -------------------------------
   -- Write_Intersecting_Labels --
   -------------------------------

   procedure Write_Intersecting_Labels
     (Intersecting_Labels : Labeled_Span_List; SGR_Code : String)
   is
      L    : Labeled_Span_Type;
      L_It : Labeled_Span_Lists.Iterator :=
        Labeled_Span_Lists.Iterate (Intersecting_Labels);
      Idx  : Integer                     := 0;
   begin
      while Labeled_Span_Lists.Has_Next (L_It) loop
         Labeled_Span_Lists.Next (L_It, L);
         Idx := Idx + 1;

         Write_Empty_Bar_Line (MAX_BAR_POS);
         Write_Str (" ");
         Write_Str ((if L.Is_Primary then SGR_Code else SGR_Note));
         Write_Int (Int (Idx));
         Write_Str (": ");
         Write_Str (L.Label.all);
         Write_Str (SGR_Reset);
         Write_Eol;
      end loop;
   end Write_Intersecting_Labels;

   ------------------------
   -- Write_File_Section --
   ------------------------

   procedure Write_File_Section
     (Sec              : File_Sections; Write_File_Name : Boolean;
      File_Name_Offset : Integer; Include_Spans : Boolean;
      SGR_Code         : String := SGR_Note)
   is
      use Lines_Lists;

      function Get_SGR_Code (L : Labeled_Span_Type) return String is
        (if L.Is_Primary then SGR_Code else SGR_Note);

      L    : Printable_Line;
      L_It : Iterator := Iterate (Sec.Lines);

      Multiple_Labeled_Spans : Boolean := False;

      Idx : Integer := 0;

      Intersecting_Labels : constant Labeled_Span_List :=
        Labeled_Span_Lists.Create;

      Prev_Line_Nr : Natural := 0;

      Within_Region_Span : Boolean := False;
   begin
      if Write_File_Name then

         --  offset the file start location for sub-diagnostics

         Write_Str (String'(1 .. File_Name_Offset => ' '));
         Write_Str ("--> " & To_String (Sec.Ptr));
         Write_Eol;
      end if;

      --  Historically SPARK does not include spans in their info messages.

      if not Include_Spans then
         return;
      end if;

      while Has_Next (L_It) loop
         Next (L_It, L);
         declare
            Line_Nr  : constant Pos    := L.Line_Nr;
            Line_Str : constant String := Trimmed_Image (Natural (Line_Nr));

            Line_Size : constant Integer :=
              Integer'Max (Line_Str'Length, MAX_BAR_POS);

            Loc    : Labeled_Span_Type;
            Loc_It : Labeled_Span_Lists.Iterator :=
              Labeled_Span_Lists.Iterate (L.Spans);

            Buf : constant Source_Buffer_Ptr :=
              Source_Text (Get_Source_File_Index (L.First));

            Region_Span : constant Labeled_Span_Type :=
              Get_Region_Span (L.Spans);

            Contains_Region_Span_Start : constant Boolean :=
              Region_Span /= No_Labeled_Span_Object
              and then Line_Nr =
                Pos (Get_Physical_Line_Number (Region_Span.Span.First));
            Contains_Region_Span_End   : constant Boolean :=
              Region_Span /= No_Labeled_Span_Object
              and then Line_Nr =
                Pos (Get_Physical_Line_Number (Region_Span.Span.Last));

            Region_Span_Color : constant String :=
              (if Region_Span /= No_Labeled_Span_Object then
                 Get_SGR_Code (Region_Span)
               else SGR_Note);
         begin
            if not Multiple_Labeled_Spans then
               Multiple_Labeled_Spans := Has_Multiple_Labeled_Spans (L);
            end if;

            --  Write an empty line with the continuation symbol if the line
            --  numbers are not contiguous

            if Prev_Line_Nr /= 0 and then Pos (Prev_Line_Nr + 1) /= Line_Nr
            then
               Write_Empty_Skip_Line (Line_Size);

               if Within_Region_Span then
                  Write_Region_Continuation (Region_Span_Color);
               end if;

               Write_Eol;
            end if;

            if Contains_Region_Span_Start then
               Within_Region_Span := True;
            end if;

            Write_Line_Marker (Line_Nr, Line_Size);

            --  Write either the region span symbol or the same number of
            --  whitespaces.

            if Contains_Region_Span_Start or Contains_Region_Span_End then
               Write_Region_Delimiter (Region_Span_Color);
            elsif Within_Region_Span then
               Write_Region_Bar (Region_Span_Color);
            else
               Write_Region_Offset;
            end if;

            --  Write the line itself

            Write_Buffer (Buf => Buf, First => L.First, Last => L.Last);

            --  Write all the spans for the line

            while Labeled_Span_Lists.Has_Next (Loc_It) loop
               Labeled_Span_Lists.Next (Loc_It, Loc);

               if Multiple_Labeled_Spans and then Loc.Label /= null then

                  --  Collect all the spans with labels to print them at the
                  --  end.

                  Labeled_Span_Lists.Append (Intersecting_Labels, Loc);

                  Idx := Idx + 1;

                  Write_Span_Labels
                    (Loc                 => Loc,
                     L                    => L,
                     Line_Size            => Line_Size,
                     Idx                  => Trimmed_Image (Idx),
                     Within_Region_Span   => Within_Region_Span,
                     SGR_Code             => Get_SGR_Code (Loc),
                     Region_Span_SGR_Code => Region_Span_Color);
               else
                  Write_Span_Labels
                    (Loc                  => Loc,
                     L                    => L,
                     Line_Size            => Line_Size,
                     Idx                  => "",
                     Within_Region_Span   => Within_Region_Span,
                     SGR_Code             => Get_SGR_Code (Loc),
                     Region_Span_SGR_Code => Region_Span_Color);
               end if;

            end loop;

            if Contains_Region_Span_End then
               Within_Region_Span := False;
            end if;

            Prev_Line_Nr := Natural (Line_Nr);
         end;
      end loop;

      Write_Intersecting_Labels (Intersecting_Labels, SGR_Code);
   end Write_File_Section;

   -------------------------
   -- Write_Labeled_Spans --
   -------------------------

   procedure Write_Labeled_Spans
     (Locations        : Labeled_Span_Id;
      Write_File_Name  : Boolean;
      File_Name_Offset : Integer;
      Include_Spans    : Boolean := True;
      SGR_Code         : String := SGR_Note)
   is
      Sections : File_Section_List := Create_File_Sections (Locations);

      Sec  : File_Sections;
      F_It : File_Section_Lists.Iterator :=
        File_Section_Lists.Iterate (Sections);
   begin
      while File_Section_Lists.Has_Next (F_It) loop
         File_Section_Lists.Next (F_It, Sec);

         Write_File_Section
           (Sec              => Sec,
            Write_File_Name  => Write_File_Name,
            File_Name_Offset => File_Name_Offset,
            Include_Spans    => Include_Spans,
            SGR_Code         => SGR_Code);
      end loop;

      File_Section_Lists.Destroy (Sections);
   end Write_Labeled_Spans;

   --------------------------
   -- Write_Error_Msg_Line --
   --------------------------

   procedure Write_Error_Msg_Line (E_Msg : Error_Msg_Object) is
      Switch_Str : constant String := Get_Doc_Switch (E_Msg);

      SGR_Code : constant String := Get_SGR_Code (E_Msg);
   begin
      Write_Str (SGR_Code);

      if not GNATprove_Mode or else E_Msg.Id /= No_Diagnostic_Id then
         Write_Str ("[" & To_String (E_Msg.Id) & "]");
      end if;

      Write_Str (" " & Kind_To_String (E_Msg) & ": ");

      Write_Str (SGR_Reset);

      Write_Str (E_Msg.Text.all);

      if Switch_Str /= "" then
         Write_Str (" " & Switch_Str);
      end if;

      if E_Msg.Warn_Err = From_Pragma then
         Write_Str (" " & Warn_As_Err_Tag);
      end if;

      Write_Eol;
   end Write_Error_Msg_Line;

   ----------------------------
   -- Should_Write_File_Name --
   ----------------------------

   function Should_Write_File_Name
     (Sub_Diag : Error_Msg_Object; Diag : Error_Msg_Object) return Boolean
   is
      Sub_Loc : constant Labeled_Span_Type :=
        Locations.Table (Primary_Location (Sub_Diag));

      Diag_Loc : constant Labeled_Span_Type :=
        Locations.Table (Primary_Location (Diag));

      function Has_Multiple_Files (Diag : Error_Msg_Object) return Boolean;

      ------------------------
      -- Has_Multiple_Files --
      ------------------------

      function Has_Multiple_Files (Diag : Error_Msg_Object) return Boolean is
         First : constant Labeled_Span_Type :=
           Locations.Table (Diag.Locations);

         File : constant String := To_File_Name (First.Span.Ptr);

         Loc_Id : Labeled_Span_Id := Diag.Locations;
         Loc    : Labeled_Span_Type;
      begin
         Loc_Id := Diag.Locations;
         while Loc_Id /= No_Labeled_Span loop
            Loc := Locations.Table (Loc_Id);

            if To_File_Name (Loc.Span.Ptr) /= File then
               return True;
            end if;

            Loc_Id := Loc.Next;
         end loop;

         return False;
      end Has_Multiple_Files;

   --  Start of processing for Should_Write_File_Name

   begin
      return
        Has_Multiple_Files (Diag)
        or else To_File_Name (Sub_Loc.Span.Ptr) /=
          To_File_Name (Diag_Loc.Span.Ptr);
   end Should_Write_File_Name;

   ------------------------
   -- Should_Write_Spans --
   ------------------------

   function Should_Write_Spans
     (Sub_Diag : Error_Msg_Object; Diag : Error_Msg_Object) return Boolean
   is
      Sub_Loc  : constant Labeled_Span_Id := Primary_Location (Sub_Diag);
      Diag_Loc : constant Labeled_Span_Id := Primary_Location (Diag);
   begin
      return
        Sub_Loc /= No_Labeled_Span and then Diag_Loc /= No_Labeled_Span
        and then Locations.Table (Sub_Loc).Span.Ptr /=
          Locations.Table (Diag_Loc).Span.Ptr;
   end Should_Write_Spans;

   ----------------
   -- Print_Edit --
   ----------------

   procedure Print_Edit (Edit : Edit_Type; Offset : Integer) is
      Buf : constant Source_Buffer_Ptr :=
        Source_Text (Get_Source_File_Index (Edit.Span.Ptr));

      Line_Nr : constant Pos := Pos (Get_Physical_Line_Number (Edit.Span.Ptr));

      Line_Fst : constant Source_Ptr := Get_Line_Start (Buf, Edit.Span.First);
      Line_Lst : constant Source_Ptr := Get_Line_End (Buf, Edit.Span.First);
   begin
      Write_Str (String'(1 .. Offset => ' '));
      Write_Str ("--> " & To_File_Name (Edit.Span.Ptr));
      Write_Eol;

      --  write the original line

      Write_Char ('-');
      Write_Line_Marker (Line_Nr, MAX_BAR_POS - 1);

      Write_Buffer (Buf => Buf, First => Line_Fst, Last => Line_Lst);

      --  write the edited line

      Write_Char ('+');
      Write_Line_Marker (Line_Nr, MAX_BAR_POS - 1);

      Write_Buffer
        (Buf => Buf, First => Line_Fst, Last => Edit.Span.First - 1);

      if Edit.Text /= null then
         Write_Str (Edit.Text.all);
      end if;

      Write_Buffer (Buf => Buf, First => Edit.Span.Last + 1, Last => Line_Lst);
   end Print_Edit;

   ---------------
   -- Print_Fix --
   ---------------

   procedure Print_Fix (Fix : Fix_Type; Offset : Integer) is
      E : Edit_Id;
   begin
      Write_Str (String'(1 .. Offset => ' '));
      Write_Str ("+ Fix: ");

      if Fix.Description /= null then
         Write_Str (Fix.Description.all);
      end if;
      Write_Eol;

      E := Fix.Edits;
      while E /= No_Edit loop
         Print_Edit (Edits.Table (E), MAX_BAR_POS - 1);

         E := Edits.Table (E).Next;
      end loop;
   end Print_Fix;

   --------------------------
   -- Print_Sub_Diagnostic --
   --------------------------

   procedure Print_Sub_Diagnostic
     (Sub_Diag : Error_Msg_Object; Diag : Error_Msg_Object; Offset : Integer)
   is
   begin
      Write_Str (String'(1 .. Offset => ' '));

      Write_Str ("+ ");

      Write_Str (Sub_Diag.Text.all);
      Write_Eol;

      if Should_Write_Spans (Sub_Diag, Diag) then
         Write_Labeled_Spans
           (Locations        => Sub_Diag.Locations,
            Write_File_Name  => Should_Write_File_Name (Sub_Diag, Diag),
            File_Name_Offset => Offset,
            Include_Spans => not GNATprove_Mode or else Sub_Diag.Kind /= Info,
            SGR_Code         => SGR_Note);
      end if;
   end Print_Sub_Diagnostic;

   ----------------------
   -- Print_Diagnostic --
   ----------------------

   procedure Print_Diagnostic (E : Error_Msg_Id) is
      E_Msg : constant Error_Msg_Object := Errors.Table (E);

      E_Next_Id : Error_Msg_Id;

      F : Fix_Id;
   begin
      --  Print the main diagnostic

      Write_Error_Msg_Line (E_Msg);

      --  Print diagnostic locations along with spans

      Write_Labeled_Spans
        (Locations        => E_Msg.Locations,
         Write_File_Name  => True,
         File_Name_Offset => 0,
         Include_Spans    => not GNATprove_Mode or else E_Msg.Kind /= Info,
         SGR_Code         => Get_SGR_Code (E_Msg));

      --  Print subdiagnostics

      E_Next_Id := E_Msg.Next;
      while E_Next_Id /= No_Error_Msg
        and then Errors.Table (E_Next_Id).Msg_Cont
      loop
         --  Print the subdiagnostic and offset the location of the file
         --  name
         Print_Sub_Diagnostic
           (Errors.Table (E_Next_Id), E_Msg, MAX_BAR_POS - 1);

         E_Next_Id := Errors.Table (E_Next_Id).Next;
      end loop;

      --  Print fixes

      F := E_Msg.Fixes;
      while F /= No_Fix loop
         Print_Fix (Fixes.Table (F), MAX_BAR_POS - 1);

         F := Fixes.Table (F).Next;
      end loop;

      --  Separate main diagnostics with a blank line

      Write_Eol;
   end Print_Diagnostic;

   --------------------------
   -- Print_Error_Messages --
   --------------------------

   procedure Print_Error_Messages is
      E : Error_Msg_Id;
   begin
      Set_Standard_Error;

      E := First_Error_Msg;
      while E /= No_Error_Msg loop

         if not Errors.Table (E).Deleted and then not Errors.Table (E).Msg_Cont
         then
            Print_Diagnostic (E);
         end if;

         E := Errors.Table (E).Next;
      end loop;

      Set_Standard_Output;
   end Print_Error_Messages;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name (Sptr : Source_Ptr) return String is
      Sfile    : constant Source_File_Index := Get_Source_File_Index (Sptr);
      Ref_Name : constant File_Name_Type    :=
        (if Full_Path_Name_For_Brief_Errors then Full_Ref_Name (Sfile)
         else Reference_Name (Sfile));

   begin
      return Get_Name_String (Ref_Name);
   end To_File_Name;

   --------------------
   -- Line_To_String --
   --------------------

   function Line_To_String (Sptr : Source_Ptr) return String is
      Line    : constant Logical_Line_Number := Get_Logical_Line_Number (Sptr);
      Img_Raw : constant String              := Int'Image (Int (Line));

   begin
      return Img_Raw (Img_Raw'First + 1 .. Img_Raw'Last);
   end Line_To_String;

   ----------------------
   -- Column_To_String --
   ----------------------

   function Column_To_String (Sptr : Source_Ptr) return String is
      Col     : constant Column_Number := Get_Column_Number (Sptr);
      Img_Raw : constant String        := Int'Image (Int (Col));

   begin
      return
        (if Col < 10 then "0" else "") &
        Img_Raw (Img_Raw'First + 1 .. Img_Raw'Last);
   end Column_To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Sptr : Source_Ptr) return String is
   begin
      return
        To_File_Name (Sptr) & ":" & Line_To_String (Sptr) & ":" &
        Column_To_String (Sptr);
   end To_String;

end Erroutc.Pretty_Emitter;
