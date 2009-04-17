------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2009, Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Run-time symbolic traceback support for IA64/VMS

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;
with System;
with System.Aux_DEC;
with System.Soft_Links;
with System.Traceback_Entries;

package body GNAT.Traceback.Symbolic is

   pragma Warnings (Off); --  ??? needs comment
   pragma Linker_Options ("--for-linker=sys$library:trace.exe");

   use System;
   use System.Aux_DEC;
   use System.Traceback_Entries;

   subtype Var_String_Buf is String (1 .. 254);

   type Var_String is record
      Curlen : Unsigned_Word := 0;
      Buf    : Var_String_Buf;
   end record;
   pragma Convention (C, Var_String);
   for Var_String'Size use 8 * 256;

   type Descriptor64 is record
      Mbo       : Unsigned_Word;
      Dtype     : Unsigned_Byte;
      Class     : Unsigned_Byte;
      Mbmo      : Unsigned_Longword;
      Maxstrlen : Integer_64;
      Pointer   : Address;
   end record;
   pragma Convention (C, Descriptor64);

   subtype Cond_Value_Type is Unsigned_Longword;

   function Symbolize
     (Current_PC    : Address;
      Filename_Dsc  : Address;
      Library_Dsc   : Address;
      Record_Number : Address;
      Image_Dsc     : Address;
      Module_Dsc    : Address;
      Routine_Dsc   : Address;
      Line_Number   : Address;
      Relative_PC   : Address) return Cond_Value_Type;
   pragma Import (C, Symbolize, "TBK$I64_SYMBOLIZE");

   function Decode_Ada_Name (Encoded_Name : String) return String;
   --  Decodes an Ada identifier name. Removes leading "_ada_" and trailing
   --  __{DIGIT}+ or ${DIGIT}+, converts other "__" to '.'

   procedure Setup_Descriptor64_Vs (Desc : out Descriptor64; Var : Address);
   --  Setup descriptor Desc for address Var

   ---------------------
   -- Decode_Ada_Name --
   ---------------------

   function Decode_Ada_Name (Encoded_Name : String) return String is
      Decoded_Name : String (1 .. Encoded_Name'Length);
      Pos          : Integer := Encoded_Name'First;
      Last         : Integer := Encoded_Name'Last;
      DPos         : Integer := 1;

   begin
      if Pos > Last then
         return "";
      end if;

      --  Skip leading _ada_

      if Encoded_Name'Length > 4
        and then Encoded_Name (Pos .. Pos + 4) = "_ada_"
      then
         Pos := Pos + 5;
      end if;

      --  Skip trailing __{DIGIT}+ or ${DIGIT}+

      if Encoded_Name (Last) in '0' .. '9' then
         for J in reverse Pos + 2 .. Last - 1 loop
            case Encoded_Name (J) is
               when '0' .. '9' =>
                  null;

               when '$' =>
                  Last := J - 1;
                  exit;

               when '_' =>
                  if Encoded_Name (J - 1) = '_' then
                     Last := J - 2;
                  end if;
                  exit;

               when others =>
                  exit;
            end case;
         end loop;
      end if;

      --  Now just copy encoded name to decoded name, converting "__" to '.'

      while Pos <= Last loop
         if Encoded_Name (Pos) = '_' and then Encoded_Name (Pos + 1) = '_'
           and then Pos /= Encoded_Name'First
         then
            Decoded_Name (DPos) := '.';
            Pos := Pos + 2;
         else
            Decoded_Name (DPos) := Encoded_Name (Pos);
            Pos := Pos + 1;
         end if;

         DPos := DPos + 1;
      end loop;

      return Decoded_Name (1 .. DPos - 1);
   end Decode_Ada_Name;

   ---------------------------
   -- Setup_Descriptor64_Vs --
   ---------------------------

   procedure Setup_Descriptor64_Vs (Desc : out Descriptor64; Var : Address) is
      K_Dtype_Vt : constant Unsigned_Byte := 37;
      K_Class_Vs : constant Unsigned_Byte := 11;
   begin
      Desc.Mbo := 1;
      Desc.Dtype := K_Dtype_Vt;
      Desc.Class := K_Class_Vs;
      Desc.Mbmo := -1;
      Desc.Maxstrlen := Integer_64 (Var_String_Buf'Length);
      Desc.Pointer := Var;
   end Setup_Descriptor64_Vs;

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String is
      Status        : Cond_Value_Type;
      Filename_Name : Var_String;
      Filename_Dsc  : Descriptor64;
      Library_Name  : Var_String;
      Library_Dsc   : Descriptor64;
      Record_Number : Integer_64;
      Image_Name    : Var_String;
      Image_Dsc     : Descriptor64;
      Module_Name   : Var_String;
      Module_Dsc    : Descriptor64;
      Routine_Name  : Var_String;
      Routine_Dsc   : Descriptor64;
      Line_Number   : Integer_64;
      Relative_PC   : Integer_64;
      Res           : String (1 .. 256 * Traceback'Length);
      Len           : Integer;

   begin
      if Traceback'Length = 0 then
         return "";
      end if;

      Len := 0;

      --  Since image computation is not thread-safe we need task lockout

      System.Soft_Links.Lock_Task.all;

      Setup_Descriptor64_Vs (Filename_Dsc, Filename_Name'Address);
      Setup_Descriptor64_Vs (Library_Dsc, Library_Name'Address);
      Setup_Descriptor64_Vs (Image_Dsc, Image_Name'Address);
      Setup_Descriptor64_Vs (Module_Dsc, Module_Name'Address);
      Setup_Descriptor64_Vs (Routine_Dsc, Routine_Name'Address);

      for J in Traceback'Range loop
         Status := Symbolize
           (PC_For (Traceback (J)),
            Filename_Dsc'Address,
            Library_Dsc'Address,
            Record_Number'Address,
            Image_Dsc'Address,
            Module_Dsc'Address,
            Routine_Dsc'Address,
            Line_Number'Address,
            Relative_PC'Address);

         declare
            First : Integer := Len + 1;
            Last  : Integer := First + 80 - 1;
            Pos   : Integer;

            Routine_Name_D : String :=
                               Decode_Ada_Name
                                 (Routine_Name.Buf
                                    (1 .. Natural (Routine_Name.Curlen)));

         begin
            Res (First .. Last) := (others => ' ');

            Res (First .. First + Natural (Image_Name.Curlen) - 1) :=
              Image_Name.Buf (1 .. Natural (Image_Name.Curlen));

            Res (First + 10 ..
                 First + 10 + Natural (Module_Name.Curlen) - 1) :=
              Module_Name.Buf (1 .. Natural (Module_Name.Curlen));

            Res (First + 30 ..
                 First + 30 + Routine_Name_D'Length - 1) :=
              Routine_Name_D;

            --  If routine name doesn't fit 20 characters, output
            --  the line number on next line at 50th position

            if Routine_Name_D'Length > 20 then
               Pos := First + 30 + Routine_Name_D'Length;
               Res (Pos) := ASCII.LF;
               Last := Pos + 80;
               Res (Pos + 1 .. Last) := (others => ' ');
               Pos := Pos + 51;
            else
               Pos := First + 50;
            end if;

            Res (Pos ..
                 Pos + Integer_64'Image (Line_Number)'Length - 1) :=
              Integer_64'Image (Line_Number);

            Res (Last) := ASCII.LF;
            Len := Last;
         end;
      end loop;

      System.Soft_Links.Unlock_Task.all;
      return Res (1 .. Len);
   end Symbolic_Traceback;

   function Symbolic_Traceback (E : Exception_Occurrence) return String is
   begin
      return Symbolic_Traceback (Tracebacks (E));
   end Symbolic_Traceback;

end GNAT.Traceback.Symbolic;
