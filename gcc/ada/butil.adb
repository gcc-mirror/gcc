------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B U T I L                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Opt;    use Opt;
with Output; use Output;
with Unchecked_Deallocation;

with GNAT; use GNAT;

with System.OS_Lib; use System.OS_Lib;

package body Butil is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Parse_Next_Unit_Name (Iter : in out Forced_Units_Iterator);
   --  Parse the name of the next available unit accessible through iterator
   --  Iter and save it in the iterator.

   function Read_Forced_Elab_Order_File return String_Ptr;
   --  Read the contents of the forced-elaboration-order file supplied to the
   --  binder via switch -f and return them as a string. Return null if the
   --  file is not available.

   --------------
   -- Has_Next --
   --------------

   function Has_Next (Iter : Forced_Units_Iterator) return Boolean is
   begin
      return Present (Iter.Unit_Name);
   end Has_Next;

   ----------------------
   -- Is_Internal_Unit --
   ----------------------

   --  Note: the reason we do not use the Fname package for this function
   --  is that it would drag too much junk into the binder.

   function Is_Internal_Unit return Boolean is
   begin
      return Is_Predefined_Unit
        or else (Name_Len > 4 and then (Name_Buffer (1 .. 5) = "gnat%"
                                          or else
                                        Name_Buffer (1 .. 5) = "gnat."));
   end Is_Internal_Unit;

   ------------------------
   -- Is_Predefined_Unit --
   ------------------------

   --  Note: the reason we do not use the Fname package for this function
   --  is that it would drag too much junk into the binder.

   function Is_Predefined_Unit return Boolean is
      L : Natural renames Name_Len;
      B : String  renames Name_Buffer;
   begin
      return    (L >  3 and then B (1 ..  4) = "ada.")
        or else (L >  6 and then B (1 ..  7) = "system.")
        or else (L > 10 and then B (1 .. 11) = "interfaces.")
        or else (L >  3 and then B (1 ..  4) = "ada%")
        or else (L >  8 and then B (1 ..  9) = "calendar%")
        or else (L >  9 and then B (1 .. 10) = "direct_io%")
        or else (L > 10 and then B (1 .. 11) = "interfaces%")
        or else (L > 13 and then B (1 .. 14) = "io_exceptions%")
        or else (L > 12 and then B (1 .. 13) = "machine_code%")
        or else (L > 13 and then B (1 .. 14) = "sequential_io%")
        or else (L >  6 and then B (1 ..  7) = "system%")
        or else (L >  7 and then B (1 ..  8) = "text_io%")
        or else (L > 20 and then B (1 .. 21) = "unchecked_conversion%")
        or else (L > 22 and then B (1 .. 23) = "unchecked_deallocation%")
        or else (L >  4 and then B (1 ..  5) = "gnat%")
        or else (L >  4 and then B (1 ..  5) = "gnat.");
   end Is_Predefined_Unit;

   --------------------------
   -- Iterate_Forced_Units --
   --------------------------

   function Iterate_Forced_Units return Forced_Units_Iterator is
      Iter : Forced_Units_Iterator;

   begin
      Iter.Order := Read_Forced_Elab_Order_File;
      Parse_Next_Unit_Name (Iter);

      return Iter;
   end Iterate_Forced_Units;

   ----------
   -- Next --
   ----------

   procedure Next
     (Iter      : in out Forced_Units_Iterator;
      Unit_Name : out Unit_Name_Type;
      Unit_Line : out Logical_Line_Number)
   is
   begin
      if not Has_Next (Iter) then
         raise Iterator_Exhausted;
      end if;

      Unit_Line := Iter.Unit_Line;
      Unit_Name := Iter.Unit_Name;
      pragma Assert (Present (Unit_Name));

      Parse_Next_Unit_Name (Iter);
   end Next;

   --------------------------
   -- Parse_Next_Unit_Name --
   --------------------------

   procedure Parse_Next_Unit_Name (Iter : in out Forced_Units_Iterator) is
      Body_Suffix : constant String   := " (body)";
      Body_Type   : constant String   := "%b";
      Body_Length : constant Positive := Body_Suffix'Length;
      Body_Offset : constant Natural  := Body_Length - 1;

      Comment_Header : constant String  := "--";
      Comment_Offset : constant Natural := Comment_Header'Length - 1;

      Spec_Suffix : constant String   := " (spec)";
      Spec_Type   : constant String   := "%s";
      Spec_Length : constant Positive := Spec_Suffix'Length;
      Spec_Offset : constant Natural  := Spec_Length - 1;

      Index : Positive            renames Iter.Order_Index;
      Line  : Logical_Line_Number renames Iter.Order_Line;
      Order : String_Ptr          renames Iter.Order;

      function At_Comment return Boolean;
      pragma Inline (At_Comment);
      --  Determine whether iterator Iter is positioned over the start of a
      --  comment.

      function At_Terminator return Boolean;
      pragma Inline (At_Terminator);
      --  Determine whether iterator Iter is positioned over a line terminator
      --  character.

      function At_Whitespace return Boolean;
      pragma Inline (At_Whitespace);
      --  Determine whether iterator Iter is positioned over a whitespace
      --  character.

      function Is_Terminator (C : Character) return Boolean;
      pragma Inline (Is_Terminator);
      --  Determine whether character C denotes a line terminator

      function Is_Whitespace (C : Character) return Boolean;
      pragma Inline (Is_Whitespace);
      --  Determine whether character C denotes a whitespace

      procedure Parse_Unit_Name;
      pragma Inline (Parse_Unit_Name);
      --  Find and parse the first available unit name

      procedure Skip_Comment;
      pragma Inline (Skip_Comment);
      --  Skip a comment by reaching a line terminator

      procedure Skip_Terminator;
      pragma Inline (Skip_Terminator);
      --  Skip a line terminator and deal with the logical line numbering

      procedure Skip_Whitespace;
      pragma Inline (Skip_Whitespace);
      --  Skip whitespace

      function Within_Order
        (Low_Offset  : Natural := 0;
         High_Offset : Natural := 0) return Boolean;
      pragma Inline (Within_Order);
      --  Determine whether index of iterator Iter is still within the range of
      --  the order string. Low_Offset may be used to inspect the area that is
      --  less than the index. High_Offset may be used to inspect the area that
      --  is greater than the index.

      ----------------
      -- At_Comment --
      ----------------

      function At_Comment return Boolean is
      begin
         --  The interator is over a comment when the index is positioned over
         --  the start of a comment header.
         --
         --    unit (spec)  --  comment
         --                 ^
         --                 Index

         return
           Within_Order (High_Offset => Comment_Offset)
             and then Order (Index .. Index + Comment_Offset) = Comment_Header;
      end At_Comment;

      -------------------
      -- At_Terminator --
      -------------------

      function At_Terminator return Boolean is
      begin
         return Within_Order and then Is_Terminator (Order (Index));
      end At_Terminator;

      -------------------
      -- At_Whitespace --
      -------------------

      function At_Whitespace return Boolean is
      begin
         return Within_Order and then Is_Whitespace (Order (Index));
      end At_Whitespace;

      -------------------
      -- Is_Terminator --
      -------------------

      function Is_Terminator (C : Character) return Boolean is
      begin
         --  Carriage return is treated intentionally as whitespace since it
         --  appears only on certain targets, while line feed is consistent on
         --  all of them.

         return C = ASCII.LF;
      end Is_Terminator;

      -------------------
      -- Is_Whitespace --
      -------------------

      function Is_Whitespace (C : Character) return Boolean is
      begin
         return
           C = ' '
             or else C = ASCII.CR   --  carriage return
             or else C = ASCII.FF   --  form feed
             or else C = ASCII.HT   --  horizontal tab
             or else C = ASCII.VT;  --  vertical tab
      end Is_Whitespace;

      ---------------------
      -- Parse_Unit_Name --
      ---------------------

      procedure Parse_Unit_Name is
         pragma Assert (not At_Comment);
         pragma Assert (not At_Terminator);
         pragma Assert (not At_Whitespace);
         pragma Assert (Within_Order);

         procedure Find_End_Index_Of_Unit_Name;
         pragma Inline (Find_End_Index_Of_Unit_Name);
         --  Position the index of iterator Iter at the last character of the
         --  first available unit name.

         ---------------------------------
         -- Find_End_Index_Of_Unit_Name --
         ---------------------------------

         procedure Find_End_Index_Of_Unit_Name is
         begin
            --  At this point the index points at the start of a unit name. The
            --  unit name may be legal, in which case it appears as:
            --
            --    unit (body)
            --
            --  However, it may also be illegal:
            --
            --    unit without suffix
            --    unit with multiple prefixes (spec)
            --
            --  In order to handle both forms, find the construct following the
            --  unit name. This is either a comment, a terminator, or the end
            --  of the order:
            --
            --    unit (body)    --  comment
            --    unit without suffix    <terminator>
            --    unit with multiple prefixes (spec)<end of order>
            --
            --  Once the construct is found, truncate the unit name by skipping
            --  all white space between the construct and the end of the unit
            --  name.

            --  Find the construct that follows the unit name

            while Within_Order loop
               if At_Comment then
                  exit;

               elsif At_Terminator then
                  exit;
               end if;

               Index := Index + 1;
            end loop;

            --  Position the index prior to the construct that follows the unit
            --  name.

            Index := Index - 1;

            --  Truncate towards the end of the unit name

            while Within_Order loop
               if At_Whitespace then
                  Index := Index - 1;
               else
                  exit;
               end if;
            end loop;
         end Find_End_Index_Of_Unit_Name;

         --  Local variables

         Start_Index : constant Positive := Index;

         End_Index : Positive;
         Is_Body   : Boolean := False;
         Is_Spec   : Boolean := False;

      --  Start of processing for Parse_Unit_Name

      begin
         Find_End_Index_Of_Unit_Name;
         End_Index := Index;

         pragma Assert (Start_Index <= End_Index);

         --  At this point the indices are positioned as follows:
         --
         --              End_Index
         --              Index
         --              v
         --    unit (spec)     --  comment
         --    ^
         --    Start_Index

         --  Rewind the index, skipping over the legal suffixes
         --
         --    Index     End_Index
         --        v     v
         --    unit (spec)     --  comment
         --    ^
         --    Start_Index

         if Within_Order (Low_Offset => Body_Offset)
           and then Order (Index - Body_Offset .. Index) = Body_Suffix
         then
            Is_Body := True;
            Index   := Index - Body_Length;

         elsif Within_Order (Low_Offset => Spec_Offset)
           and then Order (Index - Spec_Offset .. Index) = Spec_Suffix
         then
            Is_Spec := True;
            Index   := Index - Spec_Length;
         end if;

         --  Capture the line where the unit name is defined

         Iter.Unit_Line := Line;

         --  Transform the unit name to match the format recognized by the
         --  name table.

         if Is_Body then
            Iter.Unit_Name :=
              Name_Find (Order (Start_Index .. Index) & Body_Type);

         elsif Is_Spec then
            Iter.Unit_Name :=
              Name_Find (Order (Start_Index .. Index) & Spec_Type);

         --  Otherwise the unit name is illegal, so leave it as is

         else
            Iter.Unit_Name := Name_Find (Order (Start_Index .. Index));
         end if;

         --  Advance the index past the unit name
         --
         --      End_IndexIndex
         --              vv
         --    unit (spec)     --  comment
         --    ^
         --    Start_Index

         Index := End_Index + 1;
      end Parse_Unit_Name;

      ------------------
      -- Skip_Comment --
      ------------------

      procedure Skip_Comment is
      begin
         pragma Assert (At_Comment);

         while Within_Order loop
            if At_Terminator then
               exit;
            end if;

            Index := Index + 1;
         end loop;
      end Skip_Comment;

      ---------------------
      -- Skip_Terminator --
      ---------------------

      procedure Skip_Terminator is
      begin
         pragma Assert (At_Terminator);

         Index := Index + 1;
         Line  := Line  + 1;
      end Skip_Terminator;

      ---------------------
      -- Skip_Whitespace --
      ---------------------

      procedure Skip_Whitespace is
      begin
         while Within_Order loop
            if At_Whitespace then
               Index := Index + 1;
            else
               exit;
            end if;
         end loop;
      end Skip_Whitespace;

      ------------------
      -- Within_Order --
      ------------------

      function Within_Order
        (Low_Offset  : Natural := 0;
         High_Offset : Natural := 0) return Boolean
      is
      begin
         return
           Order /= null
             and then Index - Low_Offset  >= Order'First
             and then Index + High_Offset <= Order'Last;
      end Within_Order;

   --  Start of processing for Parse_Next_Unit_Name

   begin
      --  A line in the forced-elaboration-order file has the following
      --  grammar:
      --
      --    LINE ::=
      --      [WHITESPACE] UNIT_NAME [WHITESPACE] [COMMENT] TERMINATOR
      --
      --    WHITESPACE ::=
      --      <any whitespace character>
      --    | <carriage return>
      --
      --    UNIT_NAME ::=
      --      UNIT_PREFIX [WHITESPACE] UNIT_SUFFIX
      --
      --    UNIT_PREFIX ::=
      --      <any string>
      --
      --    UNIT_SUFFIX ::=
      --      (body)
      --    | (spec)
      --
      --    COMMENT ::=
      --      --  <any string>
      --
      --    TERMINATOR ::=
      --      <line feed>
      --      <end of file>
      --
      --  Items in <> brackets are semantic notions

      --  Assume that the order has no remaining units

      Iter.Unit_Line := No_Line_Number;
      Iter.Unit_Name := No_Unit_Name;

      --  Try to find the first available unit name from the current position
      --  of iteration.

      while Within_Order loop
         Skip_Whitespace;

         if At_Comment then
            Skip_Comment;

         elsif not Within_Order then
            exit;

         elsif At_Terminator then
            Skip_Terminator;

         else
            Parse_Unit_Name;
            exit;
         end if;
      end loop;
   end Parse_Next_Unit_Name;

   ---------------------------------
   -- Read_Forced_Elab_Order_File --
   ---------------------------------

   function Read_Forced_Elab_Order_File return String_Ptr is
      procedure Free is new Unchecked_Deallocation (String, String_Ptr);

      Descr    : File_Descriptor;
      Len      : Natural;
      Len_Read : Natural;
      Result   : String_Ptr;
      Success  : Boolean;

   begin
      if Force_Elab_Order_File = null then
         return null;
      end if;

      --  Obtain and sanitize a descriptor to the elaboration-order file

      Descr := Open_Read (Force_Elab_Order_File.all, Binary);

      if Descr = Invalid_FD then
         return null;
      end if;

      --  Determine the size of the file, allocate a result large enough to
      --  house its contents, and read it.

      Len := Natural (File_Length (Descr));

      if Len = 0 then
         return null;
      end if;

      Result   := new String (1 .. Len);
      Len_Read := Read (Descr, Result (1)'Address, Len);

      --  The read failed to acquire the whole content of the file

      if Len_Read /= Len then
         Free (Result);
         return null;
      end if;

      Close (Descr, Success);

      --  The file failed to close

      if not Success then
         Free (Result);
         return null;
      end if;

      return Result;
   end Read_Forced_Elab_Order_File;

   ----------------
   -- Uname_Less --
   ----------------

   function Uname_Less (U1, U2 : Unit_Name_Type) return Boolean is
   begin
      Get_Name_String (U1);

      declare
         U1_Name : constant String (1 .. Name_Len) :=
                     Name_Buffer (1 .. Name_Len);
         Min_Length : Natural;

      begin
         Get_Name_String (U2);

         if Name_Len < U1_Name'Last then
            Min_Length := Name_Len;
         else
            Min_Length := U1_Name'Last;
         end if;

         for J in 1 .. Min_Length loop
            if U1_Name (J) > Name_Buffer (J) then
               return False;
            elsif U1_Name (J) < Name_Buffer (J) then
               return True;
            end if;
         end loop;

         return U1_Name'Last < Name_Len;
      end;
   end Uname_Less;

   ---------------------
   -- Write_Unit_Name --
   ---------------------

   procedure Write_Unit_Name (U : Unit_Name_Type) is
   begin
      Get_Name_String (U);
      Write_Str (Name_Buffer (1 .. Name_Len - 2));

      if Name_Buffer (Name_Len) = 's' then
         Write_Str (" (spec)");
      else
         Write_Str (" (body)");
      end if;

      Name_Len := Name_Len + 5;
   end Write_Unit_Name;

end Butil;
