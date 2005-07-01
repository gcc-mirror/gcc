------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y M B O L S . P R O C E S S I N G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the VMS/IA64 version of this package

with Ada.IO_Exceptions;

with Ada.Unchecked_Deallocation;

separate (Symbols)
package body Processing is

   type String_Array is array (Positive range <>) of String_Access;
   type Strings_Ptr is access String_Array;

   procedure Free is
     new Ada.Unchecked_Deallocation (String_Array, Strings_Ptr);

   type Section_Header is record
      Shname   : Integer;
      Shtype   : Integer;
      Shoffset : Integer;
      Shsize   : Integer;
      Shlink   : Integer;
   end record;

   type Section_Header_Array is array (Natural range <>) of Section_Header;
   type Section_Header_Ptr is access Section_Header_Array;

   procedure Free is
     new Ada.Unchecked_Deallocation (Section_Header_Array, Section_Header_Ptr);

   -------------
   -- Process --
   -------------

   procedure Process
     (Object_File : String;
      Success     : out Boolean)
   is
      B : Byte;
      H : Integer;
      W : Integer;

      Str : String (1 .. 1000) := (others => ' ');
      Str_Last : Natural;

      Strings : Strings_Ptr;

      Shoff : Integer;
      Shnum : Integer;
      Shentsize : Integer;

      Shname   : Integer;
      Shtype   : Integer;
      Shoffset : Integer;
      Shsize   : Integer;
      Shlink   : Integer;

      Symtab_Index       : Natural := 0;
      String_Table_Index : Natural := 0;

      End_Symtab : Integer;

      Stname : Integer;
      Stinfo : Character;
      Sttype : Integer;
      Stbind : Integer;
      Stshndx : Integer;

      Section_Headers : Section_Header_Ptr;

      Offset   : Natural := 0;

      procedure Get_Byte (B : out Byte);
      procedure Get_Half (H : out Integer);
      procedure Get_Word (W : out Integer);
      procedure Reset;

      procedure Get_Byte (B : out Byte) is
      begin
         Byte_IO.Read (File, B);
         Offset := Offset + 1;
      end Get_Byte;

      procedure Get_Half (H : out Integer) is
         C1, C2 : Character;
      begin
         Get_Byte (C1); Get_Byte (C2);
         H :=
           Integer'(Character'Pos (C2)) * 256 + Integer'(Character'Pos (C1));
      end Get_Half;

      procedure Get_Word (W : out Integer) is
         H1, H2 : Integer;
      begin
         Get_Half (H1); Get_Half (H2);
         W := H2 * 256 * 256 + H1;
      end Get_Word;

      procedure Reset is
      begin
         Offset := 0;
         Byte_IO.Reset (File);
      end Reset;

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

      --  Skip ELF identification

      while Offset < 16 loop
         Get_Byte (B);
      end loop;

      --  Skip e_type

      Get_Half (H);

      --  Skip e_machine

      Get_Half (H);

      --  Skip e_version

      Get_Word (W);

      --  Skip e_entry

      for J in 1 .. 8 loop
         Get_Byte (B);
      end loop;

      --  Skip e_phoff

      for J in 1 .. 8 loop
         Get_Byte (B);
      end loop;

      Get_Word (Shoff);

      --  Skip upper half of Shoff

      for J in 1 .. 4 loop
         Get_Byte (B);
      end loop;

      --  Skip e_flags

      Get_Word (W);

      --  Skip e_ehsize

      Get_Half (H);

      --  Skip e_phentsize

      Get_Half (H);

      --  Skip e_phnum

      Get_Half (H);

      Get_Half (Shentsize);

      Get_Half (Shnum);

      Section_Headers := new Section_Header_Array (0 .. Shnum - 1);

      --  Go to Section Headers

      while Offset < Shoff loop
         Get_Byte (B);
      end loop;

      --  Reset Symtab_Index

      Symtab_Index := 0;

      for J in Section_Headers'Range loop
         --  Get the data for each Section Header

         Get_Word (Shname);
         Get_Word (Shtype);

         for K in 1 .. 16 loop
            Get_Byte (B);
         end loop;

         Get_Word (Shoffset);
         Get_Word (W);

         Get_Word (Shsize);
         Get_Word (W);

         Get_Word (Shlink);

         while (Offset - Shoff) mod Shentsize /= 0 loop
            Get_Byte (B);
         end loop;

         --  If this is the Symbol Table Section Header, record its index

         if Shtype = 2 then
            Symtab_Index := J;
         end if;

         Section_Headers (J) := (Shname, Shtype, Shoffset, Shsize, Shlink);
      end loop;

      if Symtab_Index = 0 then
         Success := False;
         return;
      end if;

      End_Symtab :=
        Section_Headers (Symtab_Index).Shoffset +
        Section_Headers (Symtab_Index).Shsize;

      String_Table_Index := Section_Headers (Symtab_Index).Shlink;
      Strings :=
        new String_Array (1 .. Section_Headers (String_Table_Index).Shsize);

      --  Go get the String Table section for the Symbol Table

      Reset;

      while Offset < Section_Headers (String_Table_Index).Shoffset loop
         Get_Byte (B);
      end loop;

      Offset := 0;

      Get_Byte (B);  --  zero

      while Offset < Section_Headers (String_Table_Index).Shsize loop
         Str_Last := 0;

         loop
            Get_Byte (B);
            if B /= ASCII.NUL then
               Str_Last := Str_Last + 1;
               Str (Str_Last) := B;

            else
               Strings (Offset - Str_Last - 1) :=
                 new String'(Str (1 .. Str_Last));
               exit;
            end if;
         end loop;
      end loop;

      --  Go get the Symbol Table

      Reset;

      while Offset < Section_Headers (Symtab_Index).Shoffset loop
         Get_Byte (B);
      end loop;

      while Offset < End_Symtab loop
         Get_Word (Stname);
         Get_Byte (Stinfo);
         Get_Byte (B);
         Get_Half (Stshndx);
         for J in 1 .. 4 loop
            Get_Word (W);
         end loop;

         Sttype := Integer'(Character'Pos (Stinfo)) mod 16;
         Stbind := Integer'(Character'Pos (Stinfo)) / 16;

         if (Sttype = 1 or else Sttype = 2)
              and then Stbind /= 0
              and then Stshndx /= 0
         then
            declare
               S_Data : Symbol_Data;
            begin
               S_Data.Name := new String'(Strings (Stname).all);

               if Sttype = 1 then
                  S_Data.Kind := Data;

               else
                  S_Data.Kind := Proc;
               end if;

               --  Put the new symbol in the table

               Symbol_Table.Increment_Last (Complete_Symbols);
               Complete_Symbols.Table
                 (Symbol_Table.Last (Complete_Symbols)) := S_Data;
            end;
         end if;
      end loop;

      --  The object file has been processed, close it

      Close (File);

      --  Free the allocated memory

      Free (Section_Headers);

      for J in Strings'Range loop
         if Strings (J) /= null then
            Free (Strings (J));
         end if;
      end loop;

      Free (Strings);

   exception
      --  For any exception, output an error message, close the object file
      --  and return with Success = False.

      when Ada.IO_Exceptions.End_Error =>
         Close (File);

      when X : others =>
         Put_Line ("unexpected exception raised while processing """
                   & Object_File & """");
         Put_Line (Exception_Information (X));
         Close (File);
         Success := False;
   end Process;

end Processing;
