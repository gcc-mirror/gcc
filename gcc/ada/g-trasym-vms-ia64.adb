------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2010, Free Software Foundation, Inc.          --
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

   --  TBK_API_PARAM as defined in TBKDEF

   type Tbk_Api_Param is record
      Length              : Unsigned_Word;
      T_Type              : Unsigned_Byte;
      Version             : Unsigned_Byte;
      Reserveda           : Unsigned_Longword;
      Faulting_Pc         : Address;
      Faulting_Fp         : Address;
      Filename_Desc       : Address;
      Library_Module_Desc : Address;
      Record_Number       : Address;
      Image_Desc          : Address;
      Module_Desc         : Address;
      Routine_Desc        : Address;
      Listing_Lineno      : Address;
      Rel_Pc              : Address;
      Image_Base_Addr     : Address;
      Module_Base_Addr    : Address;
      Malloc_Rtn          : Address;
      Free_Rtn            : Address;
      Symbolize_Flags     : Address;
      Reserved0           : Unsigned_Quadword;
      Reserved1           : Unsigned_Quadword;
      Reserved2           : Unsigned_Quadword;
   end record;
   pragma Convention (C, Tbk_Api_Param);

   K_Version : constant Unsigned_Byte := 1;
   --  Current API version

   K_Length : constant Unsigned_Word := 152;
   --  Length of the parameter

   pragma Compile_Time_Error (Tbk_Api_Param'Size = K_Length * 8,
                              "Bad length for tbk_api_param");
   --  Sanity check

   function Symbolize (Param : Address) return Cond_Value_Type;
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
      Param         : Tbk_Api_Param;
      Status        : Cond_Value_Type;
      Record_Number : Unsigned_Longword;
      Image_Name    : Var_String;
      Image_Dsc     : Descriptor64;
      Module_Name   : Var_String;
      Module_Dsc    : Descriptor64;
      Routine_Name  : Var_String;
      Routine_Dsc   : Descriptor64;
      Line_Number   : Unsigned_Longword;
      Res           : String (1 .. 256 * Traceback'Length);
      Len           : Integer;

   begin
      if Traceback'Length = 0 then
         return "";
      end if;

      Len := 0;

      --  Since image computation is not thread-safe we need task lockout

      System.Soft_Links.Lock_Task.all;

      --  Initialize descriptors

      Setup_Descriptor64_Vs (Image_Dsc, Image_Name'Address);
      Setup_Descriptor64_Vs (Module_Dsc, Module_Name'Address);
      Setup_Descriptor64_Vs (Routine_Dsc, Routine_Name'Address);

      for J in Traceback'Range loop
         --  Initialize fields in case they are not written

         Record_Number := 0;
         Line_Number := 0;
         Image_Name.Curlen := 0;
         Module_Name.Curlen := 0;
         Routine_Name.Curlen := 0;

         --  Symbolize

         Param := (Length              => K_Length,
                   T_Type              => 0,
                   Version             => K_Version,
                   Reserveda           => 0,
                   Faulting_Pc         => PC_For (Traceback (J)),
                   Faulting_Fp         => 0,
                   Filename_Desc       => Null_Address,
                   Library_Module_Desc => Null_Address,
                   Record_Number       => Record_Number'Address,
                   Image_Desc          => Image_Dsc'Address,
                   Module_Desc         => Module_Dsc'Address,
                   Routine_Desc        => Routine_Dsc'Address,
                   Listing_Lineno      => Line_Number'Address,
                   Rel_Pc              => Null_Address,
                   Image_Base_Addr     => Null_Address,
                   Module_Base_Addr    => Null_Address,
                   Malloc_Rtn          => Null_Address,
                   Free_Rtn            => Null_Address,
                   Symbolize_Flags     => Null_Address,
                   Reserved0           => (0, 0),
                   Reserved1           => (0, 0),
                   Reserved2           => (0, 0));

         Status := Symbolize (Param'Address);

         --  Check for success (marked by bit 0)

         if (Status rem 2) = 1 then

            --  Success

            if Line_Number = 0 then

               --  As GCC doesn't emit source file correlation, use record
               --  number of line number is not set

               Line_Number := Record_Number;
            end if;

            declare
               First : constant Integer := Len + 1;
               Last  : Integer := First + 80 - 1;
               Pos   : Integer;

               Routine_Name_D : constant String :=
                                  Decode_Ada_Name
                                    (Routine_Name.Buf
                                      (1 .. Natural (Routine_Name.Curlen)));

               Lineno : constant String :=
                          Unsigned_Longword'Image (Line_Number);

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

               --  If routine name doesn't fit 20 characters, output the line
               --  number on next line at 50th position.

               if Routine_Name_D'Length > 20 then
                  Pos := First + 30 + Routine_Name_D'Length;
                  Res (Pos) := ASCII.LF;
                  Last := Pos + 80;
                  Res (Pos + 1 .. Last) := (others => ' ');
                  Pos := Pos + 51;
               else
                  Pos := First + 50;
               end if;

               Res (Pos .. Pos + Lineno'Length - 1) := Lineno;

               Res (Last) := ASCII.LF;
               Len := Last;
            end;

         --  Failure (bit 0 clear)

         else
            Res (Len + 1 .. Len + 6) := "ERROR" & ASCII.LF;
            Len := Len + 6;
         end if;
      end loop;

      System.Soft_Links.Unlock_Task.all;
      return Res (1 .. Len);
   end Symbolic_Traceback;

   function Symbolic_Traceback (E : Exception_Occurrence) return String is
   begin
      return Symbolic_Traceback (Tracebacks (E));
   end Symbolic_Traceback;

end GNAT.Traceback.Symbolic;
