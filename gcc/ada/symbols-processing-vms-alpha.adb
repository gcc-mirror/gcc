------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y M B O L S . P R O C E S S I N G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2010, Free Software Foundation, Inc.         --
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

--  This is the VMS Alpha version of this package

separate (Symbols)
package body Processing is

   type Number is mod 2**16;
   --  16 bits unsigned number for number of characters

   EMH : constant Number := 8;
   --  Code for the Module Header section

   GSD : constant Number := 10;
   --  Code for the Global Symbol Definition section

   C_SYM : constant Number := 1;
   --  Code for a Symbol subsection

   V_DEF_Mask  : constant Number := 2 ** 1;
   V_NORM_Mask : constant Number := 2 ** 6;
   --  Comments ???

   B : Byte;

   Number_Of_Characters : Natural := 0;
   --  The number of characters of each section

   Native_Format : Boolean;
   --  True if records are decoded by the system (like on VMS)

   Has_Pad : Boolean;
   --  If true, a pad byte must be skipped before reading the next record

   --  The following variables are used by procedure Process when reading an
   --  object file.

   Code   : Number := 0;
   Length : Natural := 0;

   Dummy : Number;

   Nchars : Natural := 0;
   Flags  : Number  := 0;

   Symbol : String (1 .. 255);
   LSymb  : Natural;

   procedure Get (N : out Number);
   --  Read two bytes from the object file LSB first as unsigned 16 bit number

   procedure Get (N : out Natural);
   --  Read two bytes from the object file, LSByte first, as a Natural

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

   -------------
   -- Process --
   -------------

   procedure Process
     (Object_File : String;
      Success     : out Boolean)
   is
      OK : Boolean := True;

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

      --  Check the file format in case of cross-tool

      Get (Code);
      Get (Number_Of_Characters);
      Get (Dummy);

      if Code = Dummy and then Number_Of_Characters = Natural (EMH) then

         --  Looks like a cross tool

         Native_Format := False;
         Number_Of_Characters := Natural (Dummy) - 4;
         Has_Pad := (Number_Of_Characters mod 2) = 1;

      elsif Code = EMH then
         Native_Format := True;
         Number_Of_Characters := Number_Of_Characters - 6;
         Has_Pad := False;

      else
         Put_Line ("file """ & Object_File & """ is not an object file");
         Close (File);
         Success := False;
         return;
      end if;

      --  Skip the EMH section

      for J in 1 .. Number_Of_Characters loop
         Read (File, B);
      end loop;

      --  Get the different sections one by one from the object file

      while not End_Of_File (File) loop

         if not Native_Format then

            --  Skip pad byte if present

            if Has_Pad then
               Get (B);
            end if;

            --  Skip record length

            Get (Dummy);
         end if;

         Get (Code);
         Get (Number_Of_Characters);

         if not Native_Format then
            if Natural (Dummy) /= Number_Of_Characters then

               --  Format error

               raise Constraint_Error;
            end if;

            Has_Pad := (Number_Of_Characters mod 2) = 1;
         end if;

         --  The header is 4 bytes length

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

               --  If this is a symbol and the V_DEF flag is set, get symbol

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

                  --  Check if it is a symbol from a generic body

                  OK := True;

                  for J in 1 .. LSymb - 2 loop
                     if Symbol (J) = 'G' and then Symbol (J + 1) = 'P'
                       and then Symbol (J + 2) in '0' .. '9'
                     then
                        OK := False;
                        exit;
                     end if;
                  end loop;

                  if OK then

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

                        Symbol_Table.Append (Complete_Symbols, S_Data);
                     end;
                  end if;

               else
                  --  As it is not a symbol subsection, skip to the next
                  --  subsection.

                  for J in 1 .. Nchars loop
                     Read (File, B);
                     Number_Of_Characters := Number_Of_Characters - 1;
                  end loop;
               end if;

               --  Exit the GSD section when number of characters reaches zero

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

end Processing;
