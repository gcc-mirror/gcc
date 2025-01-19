------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             D I A G N O S T I C S . P R E T T Y _ E M I T T E R          --
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

with Diagnostics.Utils; use Diagnostics.Utils;
with Output;            use Output;
with Sinput;            use Sinput;
with Erroutc;           use Erroutc;

package body Diagnostics.Pretty_Emitter is

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
   type Printable_Line is record
      First   : Source_Ptr;
      --  The first character of the line

      Last    : Source_Ptr;
      --  The last character of the line

      Line_Nr : Pos;
      --  The line number

      Spans   : Labeled_Span_List;
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
      File  : String_Ptr;
      --  Name of the file

      Lines : Lines_List;
      --  Lines to be printed for the file
   end record;

   procedure Destroy (Elem : in out File_Sections);
   pragma Inline (Destroy);

   function Equals (L, R : File_Sections) return Boolean is
     (L.File /= null
       and then R.File /= null
       and then L.File.all = R.File.all);

   package File_Section_Lists is new Doubly_Linked_Lists
     (Element_Type    => File_Sections,
      "="             => Equals,
      Destroy_Element => Destroy,
      Check_Tampering => False);

   subtype File_Section_List is File_Section_Lists.Doubly_Linked_List;

   function Create_File_Sections (Spans : Labeled_Span_List)
                                  return File_Section_List;
   --  Create a list of file sections from the labeled spans that are to be
   --  printed.
   --
   --  Each file section contains a list of lines that are to be printed for
   --  the file and the spans that are applied to each of those lines.

   procedure Create_File_Section
     (Sections : in out File_Section_List;
      Loc      : Labeled_Span_Type);
   --  Create a new file section for the given labeled span.

   procedure Add_Printable_Line
     (Lines : Lines_List;
      Loc   : Labeled_Span_Type;
      S_Ptr : Source_Ptr);

   procedure Create_Printable_Line
     (Lines : Lines_List;
      Loc   : Labeled_Span_Type;
      S_Ptr : Source_Ptr);
   --  Create a new printable line for the given labeled span and add it in the
   --  correct position to the Lines list based on the line number.

   function Has_Region_Span_Start (L : Printable_Line) return Boolean;
   function Has_Region_Span_End   (L : Printable_Line) return Boolean;

   function Has_Multiple_Labeled_Spans (L : Printable_Line) return Boolean;

   procedure Write_Region_Delimiter;
   --  Write the arms signifying the start and end of a region span
   --  e.g. +--

   procedure Write_Region_Bar;
   --  Write the bar signifying the continuation of a region span
   --  e.g. |

   procedure Write_Region_Continuation;
   --  Write the continuation signifying the continuation of a region span
   --  e.g. :

   procedure Write_Region_Offset;
   --  Write a number of whitespaces equal to the size of the region span

   function Trimmed_Image (I : Natural) return String;

   procedure Write_Span_Labels (Loc : Labeled_Span_Type;
                                L   : Printable_Line;
                                Line_Size : Integer;
                                Idx : String;
                                Within_Region_Span : Boolean);

   procedure Write_File_Section (Sec : File_Sections;
                                 Write_File_Name  : Boolean;
                                 File_Name_Offset : Integer);

   procedure Write_Labeled_Spans (Spans            : Labeled_Span_List;
                                  Write_File_Name  : Boolean;
                                  File_Name_Offset : Integer);

   procedure Write_Intersecting_Labels
     (Intersecting_Labels : Labeled_Span_List);

   function Get_Line_End
     (Buf : Source_Buffer_Ptr;
      Loc : Source_Ptr) return Source_Ptr;
   --  Get the source location for the end of the line (LF) in Buf for Loc. If
   --  Loc is past the end of Buf already, return Buf'Last.

   function Get_Line_Start
     (Buf : Source_Buffer_Ptr;
      Loc : Source_Ptr) return Source_Ptr;
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
     (Buf   : Source_Buffer_Ptr;
      First : Source_Ptr;
      Last  : Source_Ptr);
   --  Output the characters from First to Last position in Buf, using
   --  Write_Buffer_Char.

   procedure Write_Buffer_Char
     (Buf : Source_Buffer_Ptr;
      Loc : Source_Ptr);
   --  Output the characters at position Loc in Buf, translating ASCII.HT
   --  in a suitable number of spaces so that the output is not modified
   --  by starting in a different column that 1.

   procedure Write_Line_Marker
     (Num   : Pos;
      Width : Positive);

   procedure Write_Empty_Bar_Line (Width : Integer);

   procedure Write_Empty_Skip_Line (Width : Integer);

   procedure Write_Error_Msg_Line (Diag : Diagnostic_Type);
   --  Write the error message line for the given diagnostic:
   --
   --  '['<Diag.Id>']' <Diag.Kind>: <Diag.Message> ['['<Diag.Switch>']']

   function Should_Write_File_Name (Sub_Diag : Sub_Diagnostic_Type;
                                    Diag : Diagnostic_Type) return Boolean;
   --  If the sub-diagnostic and the main diagnostic only point to the same
   --  file then there is no reason to add the file name to the sub-diagnostic.

   function Should_Write_Spans (Sub_Diag : Sub_Diagnostic_Type;
                                Diag : Diagnostic_Type)
                                return Boolean;
   --  Old sub-diagnostics used to have the same location as the main
   --  diagnostic in order to group them correctly. However in most cases
   --  it was not meant to point to a location but rather add an additional
   --  message to the original diagnostic.
   --
   --  If the sub-diagnostic and the main diagnostic have the same location
   --  then we should avoid printing the spans.

   procedure Print_Edit
     (Edit   : Edit_Type;
      Offset : Integer);

   procedure Print_Fix
     (Fix    : Fix_Type;
      Offset : Integer);

   procedure Print_Sub_Diagnostic
     (Sub_Diag : Sub_Diagnostic_Type;
      Diag     : Diagnostic_Type;
      Offset   : Integer);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Elem : in out Printable_Line)
   is
   begin
      --  Diagnostic elements will be freed when all the diagnostics have been
      --  emitted.
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Elem : in out File_Sections)
   is
   begin
      Free (Elem.File);
   end Destroy;

   ------------------
   -- Get_Line_End --
   ------------------

   function Get_Line_End
     (Buf : Source_Buffer_Ptr; Loc : Source_Ptr) return Source_Ptr
   is
      Cur_Loc : Source_Ptr := Source_Ptr'Min (Loc, Buf'Last);
   begin
      while Cur_Loc < Buf'Last
        and then Buf (Cur_Loc) /= ASCII.LF
      loop
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
      while Cur_Loc > Buf'First
        and then Buf (Cur_Loc - 1) /= ASCII.LF
      loop
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
      while Cur_Loc < Buf'Last
        and then Buf (Cur_Loc) = ' '
      loop
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
            Curr := Curr / 10;
         else
            Str (J) := ' ';
         end if;
      end loop;

      return Str;
   end Image;

   --------------------------------
   -- Has_Multiple_Labeled_Spans --
   --------------------------------

   function Has_Multiple_Labeled_Spans (L : Printable_Line) return Boolean
   is
      Count : Natural := 0;

      Loc : Labeled_Span_Type;
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

   ---------------------------
   -- Has_Region_Span_Start --
   ---------------------------

   function Has_Region_Span_Start (L : Printable_Line) return Boolean is
      Loc    : Labeled_Span_Type;
      Loc_It : Labeled_Span_Lists.Iterator :=
        Labeled_Span_Lists.Iterate (L.Spans);

      Has_Region_Start : Boolean := False;
   begin
      while Labeled_Span_Lists.Has_Next (Loc_It) loop
         Labeled_Span_Lists.Next (Loc_It, Loc);

         if not Has_Region_Start
            and then Loc.Is_Region
            and then L.Line_Nr =
              Pos (Get_Physical_Line_Number (Loc.Span.First))
         then
            Has_Region_Start := True;
         end if;
      end loop;
      return Has_Region_Start;
   end Has_Region_Span_Start;

   -------------------------
   -- Has_Region_Span_End --
   -------------------------

   function Has_Region_Span_End (L : Printable_Line) return Boolean is
      Loc    : Labeled_Span_Type;
      Loc_It : Labeled_Span_Lists.Iterator :=
        Labeled_Span_Lists.Iterate (L.Spans);

      Has_Region_End : Boolean := False;
   begin
      while Labeled_Span_Lists.Has_Next (Loc_It) loop
         Labeled_Span_Lists.Next (Loc_It, Loc);

         if not Has_Region_End
            and then Loc.Is_Region
            and then L.Line_Nr =
              Pos (Get_Physical_Line_Number (Loc.Span.Last))
         then
            Has_Region_End := True;
         end if;
      end loop;
      return Has_Region_End;
   end Has_Region_Span_End;

   ------------------
   -- Write_Buffer --
   ------------------

   procedure Write_Buffer
     (Buf   : Source_Buffer_Ptr;
      First : Source_Ptr;
      Last  : Source_Ptr)
   is
   begin
      for Loc in First .. Last loop
         Write_Buffer_Char (Buf, Loc);
      end loop;
   end Write_Buffer;

   -----------------------
   -- Write_Buffer_Char --
   -----------------------

   procedure Write_Buffer_Char
     (Buf : Source_Buffer_Ptr;
      Loc : Source_Ptr)
   is
   begin
      --  If the character ASCII.HT is not the last one in the file,
      --  output as many spaces as the character represents in the
      --  original source file.

      if Buf (Loc) = ASCII.HT
         and then Loc < Buf'Last
      then
         for X in Get_Column_Number (Loc) ..
                  Get_Column_Number (Loc + 1) - 1
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

   procedure Write_Line_Marker
     (Num   : Pos;
      Width : Positive)
   is
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

   procedure Write_Region_Delimiter is

   begin
      Write_Str (String'(1 .. REGION_OFFSET => ' '));
      Write_Str ("+");
      Write_Str (String'(1 .. REGION_ARM_SIZE => '-'));
   end Write_Region_Delimiter;

   ----------------------
   -- Write_Region_Bar --
   ----------------------

   procedure Write_Region_Bar is

   begin
      Write_Str (String'(1 .. REGION_OFFSET => ' '));
      Write_Str ("|");
      Write_Str (String'(1 .. REGION_ARM_SIZE => ' '));
   end Write_Region_Bar;

   -------------------------------
   -- Write_Region_Continuation --
   -------------------------------

   procedure Write_Region_Continuation is

   begin
      Write_Str (String'(1 .. REGION_OFFSET => ' '));
      Write_Str (":");
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
      L          : Printable_Line;
      L_It       : Lines_Lists.Iterator;

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
     (Lines : Lines_List;
      Loc   : Labeled_Span_Type;
      S_Ptr : Source_Ptr)
   is
      Spans : constant Labeled_Span_List := Labeled_Span_Lists.Create;

      Buf : constant Source_Buffer_Ptr :=
        Source_Text (Get_Source_File_Index (S_Ptr));

      Line_Nr  : constant Pos := Pos (Get_Physical_Line_Number (S_Ptr));

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

         if not Found_Greater_Line
           and then L.Line_Nr > New_Line.Line_Nr
         then
            Found_Greater_Line := True;
            Insert_Before_Line := L;

            Lines_Lists.Insert_Before (Lines, Insert_Before_Line, New_Line);
         end if;
      end loop;

      if Found_Greater_Line then

         --  Insert after all the lines have been iterated over to avoid the
         --  mutation lock in GNAT.Lists

         null;
      else
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
      Line_Ptr : constant Pos := Pos (Get_Physical_Line_Number (Ptr));

      --  Span start positions
      Fst      : constant Source_Ptr := Loc.Span.First;
      Line_Fst : constant Pos := Pos (Get_Physical_Line_Number (Fst));

      --  Span end positions
      Lst      : constant Source_Ptr := Loc.Span.Last;
      Line_Lst : constant Pos := Pos (Get_Physical_Line_Number (Lst));
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
          Lines => Lines));
   end Create_File_Section;

   --------------------------
   -- Create_File_Sections --
   --------------------------

   function Create_File_Sections
     (Spans : Labeled_Span_List) return File_Section_List
   is
      Loc    : Labeled_Span_Type;
      Loc_It : Labeled_Span_Lists.Iterator :=
        Labeled_Span_Lists.Iterate (Spans);

      Sections : File_Section_List := File_Section_Lists.Create;

      Sec  : File_Sections;
      F_It : File_Section_Lists.Iterator;

      File_Found : Boolean;
   begin
      while Labeled_Span_Lists.Has_Next (Loc_It) loop
         Labeled_Span_Lists.Next (Loc_It, Loc);

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
            end if;
         end loop;

         if not File_Found then
            Create_File_Section (Sections, Loc);
         end if;
      end loop;

      return Sections;
   end Create_File_Sections;

   -----------------------
   -- Write_Span_Labels --
   -----------------------

   procedure Write_Span_Labels (Loc : Labeled_Span_Type;
                                L   : Printable_Line;
                                Line_Size : Integer;
                                Idx : String;
                                Within_Region_Span : Boolean)
   is
      Span_Char : constant Character := (if Loc.Is_Primary then '~' else '-');

      Buf : constant Source_Buffer_Ptr :=
        Source_Text (Get_Source_File_Index (L.First));

      Col_L_Fst : constant Natural := Natural
        (Get_Column_Number (Get_First_Line_Char (Buf, L.First)));
      Col_L_Lst : constant Natural := Natural
        (Get_Column_Number (Get_Last_Line_Char (Buf, L.Last)));

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
        (if Line_Ptr = L.Line_Nr
         then Span_Ptr_Fst + Span_Sym'Length
         else Span_Fst);

   begin
      if not Loc.Is_Region then
         Write_Empty_Bar_Line (Line_Size);

         if Within_Region_Span then
            Write_Region_Bar;
         else
            Write_Region_Offset;
         end if;

         Write_Str (String'(1 .. Span_Fst - 1 => ' '));

         if Line_Ptr = L.Line_Nr then
            Write_Str (String'(Span_Fst .. Col_Ptr - 1 => Span_Char));
            Write_Str (Span_Sym);
         end if;

         Write_Str (String'(Span_Ptr_Lst .. Span_Lst => Span_Char));

         Write_Eol;

         --  Write the label under the line unless it is an intersecting span.
         --  In this case omit the label which will be printed later along with
         --  the index.

         if Loc.Label /= null and then Idx = "" then
            Write_Empty_Bar_Line (Line_Size);

            if Within_Region_Span then
               Write_Region_Bar;
            else
               Write_Region_Offset;
            end if;

            Write_Str (String'(1 .. Span_Fst - 1 => ' '));
            Write_Str (Loc.Label.all);
            Write_Eol;
         end if;
      else
         if Line_Lst = L.Line_Nr then
            Write_Empty_Bar_Line (Line_Size);
            Write_Str (String'(1 .. REGION_OFFSET => ' '));
            Write_Str (Loc.Label.all);
            Write_Eol;
         end if;
      end if;

   end Write_Span_Labels;

   -------------------
   -- Trimmed_Image --
   -------------------

   function Trimmed_Image (I : Natural) return String is
      Img_Raw : constant String  := Natural'Image (I);
   begin
      return Img_Raw (Img_Raw'First + 1 .. Img_Raw'Last);
   end Trimmed_Image;

   -------------------------------
   -- Write_Intersecting_Labels --
   -------------------------------

   procedure Write_Intersecting_Labels
     (Intersecting_Labels : Labeled_Span_List)
   is
      Ls    : Labeled_Span_Type;
      Ls_It : Labeled_Span_Lists.Iterator :=
        Labeled_Span_Lists.Iterate (Intersecting_Labels);
      Idx   : Integer := 0;
   begin
      while Labeled_Span_Lists.Has_Next (Ls_It) loop
         Labeled_Span_Lists.Next (Ls_It, Ls);
         Idx := Idx + 1;

         Write_Empty_Bar_Line (MAX_BAR_POS);
         Write_Str (" ");
         Write_Int (Int (Idx));
         Write_Str (": ");
         Write_Str (Ls.Label.all);
         Write_Eol;
      end loop;
   end Write_Intersecting_Labels;

   ------------------------
   -- Write_File_Section --
   ------------------------

   procedure Write_File_Section (Sec              : File_Sections;
                                 Write_File_Name  : Boolean;
                                 File_Name_Offset : Integer)
   is
      use Lines_Lists;

      L : Printable_Line;
      L_It : Iterator := Iterate (Sec.Lines);

      --  The error should be included in the first (primary) span of the file.
      Loc : constant Labeled_Span_Type :=
         Labeled_Span_Lists.First (Lines_Lists.First (Sec.Lines).Spans);

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
         Write_Str ("--> " & To_String (Loc.Span.Ptr));
         Write_Eol;
      end if;

      while Has_Next (L_It) loop
         Next (L_It, L);
         declare
            Line_Nr  : constant Pos := L.Line_Nr;
            Line_Str : constant String := Trimmed_Image (Natural (Line_Nr));

            Line_Size : constant Integer :=
              Integer'Max (Line_Str'Length, MAX_BAR_POS);

            Loc : Labeled_Span_Type;
            Loc_It : Labeled_Span_Lists.Iterator :=
              Labeled_Span_Lists.Iterate (L.Spans);

            Buf : constant Source_Buffer_Ptr :=
              Source_Text (Get_Source_File_Index (L.First));

            Contains_Region_Span_Start : constant Boolean :=
              Has_Region_Span_Start (L);
            Contains_Region_Span_End   : constant Boolean :=
              Has_Region_Span_End (L);
         begin
            if not Multiple_Labeled_Spans then
               Multiple_Labeled_Spans :=  Has_Multiple_Labeled_Spans (L);
            end if;

            --  Write an empty line with the continuation symbol if the line
            --  numbers are not contiguous

            if Prev_Line_Nr /= 0
              and then Pos (Prev_Line_Nr + 1) /= Line_Nr
            then
               Write_Empty_Skip_Line (Line_Size);

               if Within_Region_Span then
                  Write_Region_Continuation;
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
               Write_Region_Delimiter;
            elsif Within_Region_Span then
               Write_Region_Bar;
            else
               Write_Region_Offset;
            end if;

            --  Write the line itself

            Write_Buffer
               (Buf   => Buf,
                First => L.First,
                Last  => L.Last);

            --  Write all the spans for the line

            while Labeled_Span_Lists.Has_Next (Loc_It) loop
               Labeled_Span_Lists.Next (Loc_It, Loc);

               if Multiple_Labeled_Spans
                 and then Loc.Label /= null
               then

                  --  Collect all the spans with labels to print them at the
                  --  end.

                  Labeled_Span_Lists.Append (Intersecting_Labels, Loc);

                  Idx := Idx + 1;

                  Write_Span_Labels (Loc,
                                     L,
                                     Line_Size,
                                     Trimmed_Image (Idx),
                                     Within_Region_Span);
               else
                  Write_Span_Labels (Loc,
                                     L,
                                     Line_Size,
                                     "",
                                     Within_Region_Span);
               end if;

            end loop;

            if Contains_Region_Span_End then
               Within_Region_Span := False;
            end if;

            Prev_Line_Nr := Natural (Line_Nr);
         end;
      end loop;

      Write_Intersecting_Labels (Intersecting_Labels);
   end Write_File_Section;

   -------------------------
   -- Write_Labeled_Spans --
   -------------------------

   procedure Write_Labeled_Spans (Spans            : Labeled_Span_List;
                                  Write_File_Name  : Boolean;
                                  File_Name_Offset : Integer)
   is
      Sections : File_Section_List := Create_File_Sections (Spans);

      Sec  : File_Sections;
      F_It : File_Section_Lists.Iterator :=
        File_Section_Lists.Iterate (Sections);
   begin
      while File_Section_Lists.Has_Next (F_It) loop
         File_Section_Lists.Next (F_It, Sec);

         Write_File_Section
           (Sec, Write_File_Name, File_Name_Offset);
      end loop;

      File_Section_Lists.Destroy (Sections);
   end Write_Labeled_Spans;

   --------------------------
   -- Write_Error_Msg_Line --
   --------------------------

   procedure Write_Error_Msg_Line (Diag : Diagnostic_Type) is
      Switch_Str : constant String := Get_Doc_Switch (Diag);

      Kind_Str : constant String := Kind_To_String (Diag);

      SGR_Code : constant String :=
        (if Kind_Str = "error"      then SGR_Error
         elsif Kind_Str = "warning" then SGR_Warning
         elsif Kind_Str = "info"    then SGR_Note
         else                            SGR_Reset);
   begin
      Write_Str (SGR_Code);

      Write_Str ("[" & To_String (Diag.Id) & "]");

      Write_Str (" " & Kind_To_String (Diag) & ": ");

      Write_Str (SGR_Reset);

      Write_Str (Diag.Message.all);

      if Switch_Str /= "" then
         Write_Str (" " & Switch_Str);
      end if;

      if Diag.Warn_Err then
         Write_Str (" [warning-as-error]");
      end if;

      Write_Eol;
   end Write_Error_Msg_Line;

   ----------------------------
   -- Should_Write_File_Name --
   ----------------------------

   function Should_Write_File_Name (Sub_Diag : Sub_Diagnostic_Type;
                                    Diag : Diagnostic_Type)
                                    return Boolean
   is
      Sub_Loc  : constant Labeled_Span_Type := Primary_Location (Sub_Diag);
      Diag_Loc : constant Labeled_Span_Type := Primary_Location (Diag);

      function Has_Multiple_Files (Spans : Labeled_Span_List) return Boolean;

      ------------------------
      -- Has_Multiple_Files --
      ------------------------

      function Has_Multiple_Files
        (Spans : Labeled_Span_List) return Boolean
      is
         First : constant Labeled_Span_Type :=
           Labeled_Span_Lists.First (Spans);

         File : constant String := To_File_Name (First.Span.Ptr);

         Loc : Labeled_Span_Type;
         It : Labeled_Span_Lists.Iterator :=
           Labeled_Span_Lists.Iterate (Spans);

      begin
         while Labeled_Span_Lists.Has_Next (It) loop
            Labeled_Span_Lists.Next (It, Loc);

            if To_File_Name (Loc.Span.Ptr) /= File then
               return True;
            end if;
         end loop;
         return False;
      end Has_Multiple_Files;
   begin
      return
        Has_Multiple_Files (Diag.Locations)
        or else To_File_Name (Sub_Loc.Span.Ptr) /=
          To_File_Name (Diag_Loc.Span.Ptr);
   end Should_Write_File_Name;

   ------------------------
   -- Should_Write_Spans --
   ------------------------

   function Should_Write_Spans (Sub_Diag : Sub_Diagnostic_Type;
                                Diag : Diagnostic_Type)
                                return Boolean
   is
      Sub_Loc  : constant Labeled_Span_Type := Primary_Location (Sub_Diag);
      Diag_Loc : constant Labeled_Span_Type := Primary_Location (Diag);
   begin
      return Sub_Loc /= No_Labeled_Span
        and then Diag_Loc /= No_Labeled_Span
        and then Sub_Loc.Span.Ptr /= Diag_Loc.Span.Ptr;
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

      Write_Buffer
         (Buf   => Buf,
          First => Line_Fst,
          Last  => Line_Lst);

      --  write the edited line

      Write_Char ('+');
      Write_Line_Marker (Line_Nr, MAX_BAR_POS - 1);

      Write_Buffer
        (Buf   => Buf,
         First => Line_Fst,
         Last  => Edit.Span.First - 1);

      if Edit.Text /= null then
         Write_Str (Edit.Text.all);
      end if;

      Write_Buffer
        (Buf   => Buf,
         First => Edit.Span.Last + 1,
         Last  => Line_Lst);

   end Print_Edit;

   ---------------
   -- Print_Fix --
   ---------------

   procedure Print_Fix (Fix : Fix_Type; Offset : Integer) is
      use Edit_Lists;
   begin
      Write_Str (String'(1 .. Offset => ' '));
      Write_Str ("+ Fix: ");

      if Fix.Description /= null then
         Write_Str (Fix.Description.all);
      end if;
      Write_Eol;

      if Present (Fix.Edits) then
         declare
            Edit : Edit_Type;

            It : Iterator := Iterate (Fix.Edits);
         begin
            while Has_Next (It) loop
               Next (It, Edit);

               Print_Edit (Edit, MAX_BAR_POS - 1);
            end loop;
         end;
      end if;
   end Print_Fix;

   --------------------------
   -- Print_Sub_Diagnostic --
   --------------------------

   procedure Print_Sub_Diagnostic
     (Sub_Diag : Sub_Diagnostic_Type;
      Diag     : Diagnostic_Type;
      Offset   : Integer)
   is
   begin
      Write_Str (String'(1 .. Offset => ' '));

      if Sub_Diag.Kind = Suggestion then
         Write_Str ("+ Suggestion: ");
      else
         Write_Str ("+ ");
      end if;

      Write_Str (Sub_Diag.Message.all);
      Write_Eol;

      if Should_Write_Spans (Sub_Diag, Diag) then
         Write_Labeled_Spans (Sub_Diag.Locations,
                              Should_Write_File_Name (Sub_Diag, Diag),
                              Offset);
      end if;
   end Print_Sub_Diagnostic;

   ----------------------
   -- Print_Diagnostic --
   ----------------------

   procedure Print_Diagnostic (Diag : Diagnostic_Type) is

   begin
      --  Print the main diagnostic

      Write_Error_Msg_Line (Diag);

      --  Print diagnostic locations along with spans

      Write_Labeled_Spans (Diag.Locations, True, 0);

      --  Print subdiagnostics

      if Sub_Diagnostic_Lists.Present (Diag.Sub_Diagnostics) then
         declare
            use Sub_Diagnostic_Lists;
            Sub_Diag : Sub_Diagnostic_Type;

            It : Iterator := Iterate (Diag.Sub_Diagnostics);
         begin
            while Has_Next (It) loop
               Next (It, Sub_Diag);

               --  Print the subdiagnostic and offset the location of the file
               --  name

               Print_Sub_Diagnostic (Sub_Diag, Diag, MAX_BAR_POS - 1);
            end loop;
         end;
      end if;

      --  Print fixes

      if Fix_Lists.Present (Diag.Fixes) then
         declare
            use Fix_Lists;
            Fix : Fix_Type;

            It : Iterator := Iterate (Diag.Fixes);
         begin
            while Has_Next (It) loop
               Next (It, Fix);

               Print_Fix (Fix, MAX_BAR_POS - 1);
            end loop;
         end;
      end if;

      --  Separate main diagnostics with a blank line

      Write_Eol;

   end Print_Diagnostic;
end Diagnostics.Pretty_Emitter;
