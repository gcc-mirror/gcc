------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S Y M B O L S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003 Free Software Foundation, Inc.               --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the VMS version of this package

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Sequential_IO;
with Ada.Text_IO;       use Ada.Text_IO;

package body Symbols is

   Case_Sensitive  : constant String := "case_sensitive=";
   Symbol_Vector   : constant String := "SYMBOL_VECTOR=(";
   Equal_Data      : constant String := "=DATA)";
   Equal_Procedure : constant String := "=PROCEDURE)";
   Gsmatch         : constant String := "gsmatch=equal,";

   Symbol_File_Name : String_Access := null;
   --  Name of the symbol file

   Sym_Policy : Policy := Autonomous;
   --  The symbol policy. Set by Initialize

   Major_ID : Integer := 1;
   --  The Major ID. May be modified by Initialize if Library_Version is
   --  specified or if it is read from the reference symbol file.

   Soft_Major_ID : Boolean := True;
   --  False if library version is specified in procedure Initialize.
   --  When True, Major_ID may be modified if found in the reference symbol
   --  file.

   Minor_ID : Natural := 0;
   --  The Minor ID. May be modified if read from the reference symbol file

   Soft_Minor_ID : Boolean := True;
   --  False if symbol policy is Autonomous, if library version is specified
   --  in procedure Initialize and is not the same as the major ID read from
   --  the reference symbol file. When True, Minor_ID may be increased in
   --  Compliant symbol policy.

   subtype Byte is Character;
   --  Object files are stream of bytes, but some of these bytes, those for
   --  the names of the symbols, are ASCII characters.

   package Byte_IO is new Ada.Sequential_IO (Byte);
   use Byte_IO;

   type Number is mod 2**16;
   --  16 bits unsigned number for number of characters

   GSD : constant Number := 10;
   --  Code for the Global Symbol Definition section

   C_SYM : constant Number := 1;
   --  Code for a Symbol subsection

   V_DEF_Mask  : constant Number := 2**1;
   V_NORM_Mask : constant Number := 2**6;

   File : Byte_IO.File_Type;
   --  Each object file is read as a stream of bytes (characters)

   B : Byte;

   Number_Of_Characters : Natural := 0;
   --  The number of characters of each section

   --  The following variables are used by procedure Process when reading an
   --  object file.

   Code   : Number := 0;
   Length : Natural := 0;

   Dummy : Number;

   Nchars : Natural := 0;
   Flags  : Number  := 0;

   Symbol : String (1 .. 255);
   LSymb  : Natural;

   function Equal (Left, Right : Symbol_Data) return Boolean;
   --  Test for equality of symbols

   procedure Get (N : out Number);
   --  Read two bytes from the object file LSB first as unsigned 16 bit number

   procedure Get (N : out Natural);
   --  Read two bytes from the object file, LSByte first, as a Natural


   function Image (N : Integer) return String;
   --  Returns the image of N, without the initial space

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Symbol_Data) return Boolean is
   begin
      return Left.Name /= null and then
             Right.Name /= null and then
             Left.Name.all = Right.Name.all and then
             Left.Kind = Right.Kind and then
             Left.Present = Right.Present;
   end Equal;

   ---------
   -- Get --
   ---------

   procedure Get (N : out Number) is
      C : Byte;
      LSByte : Number;
   begin
      Read (File, C);
      LSByte := Byte'Pos (C);
      Read (File, C);
      N := LSByte + (256 * Byte'Pos (C));
   end Get;

   procedure Get (N : out Natural) is
      Result : Number;
   begin
      Get (Result);
      N := Natural (Result);
   end Get;

   -----------
   -- Image --
   -----------

   function Image (N : Integer) return String is
      Result : constant String := N'Img;
   begin
      if Result (Result'First) = ' ' then
         return Result (Result'First + 1 .. Result'Last);

      else
         return Result;
      end if;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Symbol_File   : String;
      Reference     : String;
      Symbol_Policy : Policy;
      Quiet         : Boolean;
      Version       : String;
      Success       : out Boolean)
   is
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 1_000);
      Last : Natural;

   begin
      --  Record the symbol file name

      Symbol_File_Name := new String'(Symbol_File);

      --  Record the policy

      Sym_Policy := Symbol_Policy;

      --  Record the version (Major ID)

      if Version = "" then
         Major_ID := 1;
         Soft_Major_ID := True;

      else
         begin
            Major_ID := Integer'Value (Version);
            Soft_Major_ID := False;

            if Major_ID <= 0 then
               raise Constraint_Error;
            end if;

         exception
            when Constraint_Error =>
               if not Quiet then
                  Put_Line ("Version """ & Version & """ is illegal.");
                  Put_Line ("On VMS, version must be a positive number");
               end if;

               Success := False;
               return;
         end;
      end if;

      Minor_ID := 0;
      Soft_Minor_ID := Sym_Policy /= Autonomous;

      --  Empty the symbol tables

      Symbol_Table.Set_Last (Original_Symbols, 0);
      Symbol_Table.Set_Last (Complete_Symbols, 0);

      --  Assume that everything will be fine

      Success := True;

      --  If policy is not autonomous, attempt to read the reference file

      if Sym_Policy /= Autonomous then
         begin
            Open (File, In_File, Reference);

         exception
            when Ada.Text_IO.Name_Error =>
               return;

            when X : others =>
               if not Quiet then
                  Put_Line ("could not open """ & Reference & """");
                  Put_Line (Exception_Message (X));
               end if;

               Success := False;
               return;
         end;

         --  Read line by line

         while not End_Of_File (File) loop
            Get_Line (File, Line, Last);

            --  Ignore empty lines

            if Last = 0 then
               null;

            --  Ignore lines starting with "case_sensitive="

            elsif Last > Case_Sensitive'Length
              and then Line (1 .. Case_Sensitive'Length) = Case_Sensitive
            then
               null;

            --  Line starting with "SYMBOL_VECTOR=("

            elsif Last > Symbol_Vector'Length
              and then Line (1 .. Symbol_Vector'Length) = Symbol_Vector
            then

               --  SYMBOL_VECTOR=(<symbol>=DATA)

               if Last > Symbol_Vector'Length + Equal_Data'Length and then
                 Line (Last - Equal_Data'Length + 1 .. Last) = Equal_Data
               then
                  Symbol_Table.Increment_Last (Original_Symbols);
                  Original_Symbols.Table
                    (Symbol_Table.Last (Original_Symbols)) :=
                      (Name =>
                         new String'(Line (Symbol_Vector'Length + 1 ..
                                           Last - Equal_Data'Length)),
                       Kind => Data,
                       Present => True);

               --  SYMBOL_VECTOR=(<symbol>=PROCEDURE)

               elsif Last > Symbol_Vector'Length + Equal_Procedure'Length
                 and then
                  Line (Last - Equal_Procedure'Length + 1 .. Last) =
                                                              Equal_Procedure
               then
                  Symbol_Table.Increment_Last (Original_Symbols);
                  Original_Symbols.Table
                    (Symbol_Table.Last (Original_Symbols)) :=
                    (Name =>
                       new String'(Line (Symbol_Vector'Length + 1 ..
                                         Last - Equal_Procedure'Length)),
                     Kind => Proc,
                     Present => True);

               --  Anything else is incorrectly formatted

               else
                  if not Quiet then
                     Put_Line ("symbol file """ & Reference &
                               """ is incorrectly formatted:");
                     Put_Line ("""" & Line (1 .. Last) & """");
                  end if;

                  Close (File);
                  Success := False;
                  return;
               end if;

            --  Lines with "gsmatch=equal,<Major_ID>,<Minor_Id>

            elsif Last > Gsmatch'Length
              and then Line (1 .. Gsmatch'Length) = Gsmatch
            then
               declare
                  Start  : Positive := Gsmatch'Length + 1;
                  Finish : Positive := Start;
                  OK     : Boolean  := True;
                  ID     : Integer;

               begin
                  loop
                     if Line (Finish) not in '0' .. '9'
                       or else Finish >= Last - 1
                     then
                        OK := False;
                        exit;
                     end if;

                     exit when Line (Finish + 1) = ',';

                     Finish := Finish + 1;
                  end loop;

                  if OK then
                     ID := Integer'Value (Line (Start .. Finish));
                     OK := ID /= 0;

                     --  If Soft_Major_ID is True, it means that
                     --  Library_Version was not specified.

                     if Soft_Major_ID then
                        Major_ID := ID;

                     --  If the Major ID in the reference file is different
                     --  from the Library_Version, then the Minor ID will be 0
                     --  because there is no point in taking the Minor ID in
                     --  the reference file, or incrementing it. So, we set
                     --  Soft_Minor_ID to False, so that we don't modify
                     --  the Minor_ID later.

                     elsif Major_ID /= ID then
                        Soft_Minor_ID := False;
                     end if;

                     Start := Finish + 2;
                     Finish := Start;

                     loop
                        if Line (Finish) not in '0' .. '9' then
                           OK := False;
                           exit;
                        end if;

                        exit when Finish = Last;

                        Finish := Finish + 1;
                     end loop;

                     --  Only set Minor_ID if Soft_Minor_ID is True (see above)

                     if OK and then Soft_Minor_ID then
                        Minor_ID := Integer'Value (Line (Start .. Finish));
                     end if;
                  end if;

                  --  If OK is not True, that means the line is not correctly
                  --  formatted.

                  if not OK then
                     if not Quiet then
                        Put_Line ("symbol file """ & Reference &
                                  """ is incorrectly formatted");
                        Put_Line ("""" & Line (1 .. Last) & """");
                     end if;

                     Close (File);
                     Success := False;
                     return;
                  end if;
               end;

            --  Anything else is incorrectly formatted

            else
               if not Quiet then
                  Put_Line ("unexpected line in symbol file """ &
                            Reference & """");
                  Put_Line ("""" & Line (1 .. Last) & """");
               end if;

               Close (File);
               Success := False;
               return;
            end if;
         end loop;

         Close (File);
      end if;
   end Initialize;

   -------------
   -- Process --
   -------------

   procedure Process
     (Object_File : String;
      Success     : out Boolean)
   is
   begin
      --  Open the object file with Byte_IO. Return with Success = False if
      --  this fails.

      begin
         Open (File, In_File, Object_File);
      exception
         when others =>
            Put_Line
              ("*** Unable to open object file """ & Object_File & """");
            Success := False;
            return;
      end;

      --  Assume that the object file has a correct format

      Success := True;

      --  Get the different sections one by one from the object file

      while not End_Of_File (File) loop

         Get (Code);
         Get (Number_Of_Characters);
         Number_Of_Characters := Number_Of_Characters - 4;

         --  If this is not a Global Symbol Definition section, skip to the
         --  next section.

         if Code /= GSD then

            for J in 1 .. Number_Of_Characters loop
               Read (File, B);
            end loop;

         else

            --  Skip over the next 4 bytes

            Get (Dummy);
            Get (Dummy);
            Number_Of_Characters := Number_Of_Characters - 4;

            --  Get each subsection in turn

            loop
               Get (Code);
               Get (Nchars);
               Get (Dummy);
               Get (Flags);
               Number_Of_Characters := Number_Of_Characters - 8;
               Nchars := Nchars - 8;

               --  If this is a symbol and the V_DEF flag is set, get the
               --  symbol.

               if Code = C_SYM and then ((Flags and V_DEF_Mask) /= 0) then
                  --  First, reach the symbol length

                  for J in 1 .. 25 loop
                     Read (File, B);
                     Nchars := Nchars - 1;
                     Number_Of_Characters := Number_Of_Characters - 1;
                  end loop;

                  Length := Byte'Pos (B);
                  LSymb := 0;

                  --  Get the symbol characters

                  for J in 1 .. Nchars loop
                     Read (File, B);
                     Number_Of_Characters := Number_Of_Characters - 1;
                     if Length > 0 then
                        LSymb := LSymb + 1;
                        Symbol (LSymb) := B;
                        Length := Length - 1;
                     end if;
                  end loop;

                  --  Create the new Symbol

                  declare
                     S_Data : Symbol_Data;
                  begin
                     S_Data.Name := new String'(Symbol (1 .. LSymb));

                     --  The symbol kind (Data or Procedure) depends on the
                     --  V_NORM flag.

                     if (Flags and V_NORM_Mask) = 0 then
                        S_Data.Kind := Data;

                     else
                        S_Data.Kind := Proc;
                     end if;

                     --  Put the new symbol in the table

                     Symbol_Table.Increment_Last (Complete_Symbols);
                     Complete_Symbols.Table
                       (Symbol_Table.Last (Complete_Symbols)) := S_Data;
                  end;

               else
                  --  As it is not a symbol subsection, skip to the next
                  --  subsection.

                  for J in 1 .. Nchars loop
                     Read (File, B);
                     Number_Of_Characters := Number_Of_Characters - 1;
                  end loop;
               end if;

               --  Exit the GSD section when number of characters reaches 0

               exit when Number_Of_Characters = 0;
            end loop;
         end if;
      end loop;

      --  The object file has been processed, close it

      Close (File);

   exception
      --  For any exception, output an error message, close the object file
      --  and return with Success = False.

      when X : others =>
         Put_Line ("unexpected exception raised while processing """
                   & Object_File & """");
         Put_Line (Exception_Information (X));
         Close (File);
         Success := False;
   end Process;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Quiet   : Boolean;
      Success : out Boolean)
   is
      File   : Ada.Text_IO.File_Type;
      --  The symbol file

      S_Data : Symbol_Data;
      --  A symbol

      Cur    : Positive := 1;
      --  Most probable index in the Complete_Symbols of the current symbol
      --  in Original_Symbol.

      Found  : Boolean;

   begin
      --  Nothing to be done if Initialize has never been called

      if Symbol_File_Name = null then
         Success := False;

      else

         --  First find if the symbols in the reference symbol file are also
         --  in the object files. Note that this is not done if the policy is
         --  Autonomous, because no reference symbol file has been read.

         --  Expect the first symbol in the symbol file to also be the first
         --  in Complete_Symbols.

         Cur := 1;

         for Index_1 in 1 .. Symbol_Table.Last (Original_Symbols) loop
            S_Data := Original_Symbols.Table (Index_1);
            Found := False;

            First_Object_Loop :
            for Index_2 in Cur .. Symbol_Table.Last (Complete_Symbols) loop
               if Equal (S_Data, Complete_Symbols.Table (Index_2)) then
                  Cur := Index_2 + 1;
                  Complete_Symbols.Table (Index_2).Present := False;
                  Found := True;
                  exit First_Object_Loop;
               end if;
            end loop First_Object_Loop;

            --  If the symbol could not be found between Cur and Last, try
            --  before Cur.

            if not Found then
               Second_Object_Loop :
               for Index_2 in 1 .. Cur - 1 loop
                  if Equal (S_Data, Complete_Symbols.Table (Index_2)) then
                     Cur := Index_2 + 1;
                     Complete_Symbols.Table (Index_2).Present := False;
                     Found := True;
                     exit Second_Object_Loop;
                  end if;
               end loop Second_Object_Loop;
            end if;

            --  If the symbol is not found, mark it as such in the table

            if not Found then
               if (not Quiet) or else Sym_Policy = Controlled then
                  Put_Line ("symbol """ & S_Data.Name.all &
                            """ is no longer present in the object files");
               end if;

               if Sym_Policy = Controlled then
                  Success := False;
                  return;

               elsif Soft_Minor_ID then
                  Minor_ID := Minor_ID + 1;
                  Soft_Minor_ID := False;
               end if;

               Original_Symbols.Table (Index_1).Present := False;
               Free (Original_Symbols.Table (Index_1).Name);

               if Soft_Minor_ID then
                  Minor_ID := Minor_ID + 1;
                  Soft_Minor_ID := False;
               end if;
            end if;
         end loop;

         --  Append additional symbols, if any, to the Original_Symbols table

         for Index in 1 .. Symbol_Table.Last (Complete_Symbols) loop
            S_Data := Complete_Symbols.Table (Index);

            if S_Data.Present then

               if Sym_Policy = Controlled then
                  Put_Line ("symbol """ & S_Data.Name.all &
                            """ is not in the reference symbol file");
                  Success := False;
                  return;

               elsif Soft_Minor_ID then
                  Minor_ID := Minor_ID + 1;
                  Soft_Minor_ID := False;
               end if;

               Symbol_Table.Increment_Last (Original_Symbols);
               Original_Symbols.Table (Symbol_Table.Last (Original_Symbols)) :=
                 S_Data;
               Complete_Symbols.Table (Index).Present := False;
            end if;
         end loop;

         --  Create the symbol file

         Create (File, Ada.Text_IO.Out_File, Symbol_File_Name.all);

         Put (File, Case_Sensitive);
         Put_Line (File, "yes");

         --  Put a line in the symbol file for each symbol in the symbol table

         for Index in 1 .. Symbol_Table.Last (Original_Symbols) loop
            if Original_Symbols.Table (Index).Present then
               Put (File, Symbol_Vector);
               Put (File, Original_Symbols.Table (Index).Name.all);

               if Original_Symbols.Table (Index).Kind = Data then
                  Put_Line (File, Equal_Data);

               else
                  Put_Line (File, Equal_Procedure);
               end if;

               Free (Original_Symbols.Table (Index).Name);
            end if;
         end loop;

         Put (File, Case_Sensitive);
         Put_Line (File, "NO");

         --  Put the version IDs

         Put (File, Gsmatch);
         Put (File, Image (Major_ID));
         Put (File, ',');
         Put_Line  (File, Image (Minor_ID));

         --  And we are done

         Close (File);

         --  Reset both tables

         Symbol_Table.Set_Last (Original_Symbols, 0);
         Symbol_Table.Set_Last (Complete_Symbols, 0);

         --  Clear the symbol file name

         Free (Symbol_File_Name);

         Success := True;
      end if;

   exception
      when X : others =>
         Put_Line ("unexpected exception raised while finalizing """
                   & Symbol_File_Name.all & """");
         Put_Line (Exception_Information (X));
         Success := False;
   end Finalize;

end Symbols;
