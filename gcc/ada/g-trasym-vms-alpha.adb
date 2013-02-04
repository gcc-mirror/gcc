------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1999-2012, Free Software Foundation, Inc.        --
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

--  Run-time symbolic traceback support for Alpha/VMS

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;
with Interfaces.C;
with System;
with System.Aux_DEC;
with System.Soft_Links;
with System.Traceback_Entries;

package body GNAT.Traceback.Symbolic is

   pragma Warnings (Off); --  Needs comment ???
   pragma Linker_Options ("--for-linker=sys$library:trace.exe");

   use Interfaces.C;
   use System;
   use System.Aux_DEC;
   use System.Traceback_Entries;

   subtype User_Arg_Type is Unsigned_Longword;
   subtype Cond_Value_Type is Unsigned_Longword;

   type ASCIC is record
      Count : unsigned_char;
      Data  : char_array (1 .. 255);
   end record;
   pragma Convention (C, ASCIC);

   for ASCIC use record
      Count at 0 range 0 .. 7;
      Data  at 1 range 0 .. 8 * 255 - 1;
   end record;
   for ASCIC'Size use 8 * 256;

   function Fetch_ASCIC is new Fetch_From_Address (ASCIC);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Dummy_User_Act_Proc
     (Msgvec : Address       := Null_Address;
      Actrtn : Address       := Null_Address;
      Facnam : Address       := Null_Address;
      Actprm : User_Arg_Type := 0) return Cond_Value_Type;
   --  Dummy routine with SYS$PUTMSG signature

   procedure Symbolize
     (Status         : out Cond_Value_Type;
      Current_PC     : Address;
      Adjusted_PC    : Address;
      Current_FP     : Address;
      Current_R26    : Address;
      Image_Name     : out Address;
      Module_Name    : out Address;
      Routine_Name   : out Address;
      Line_Number    : out Integer;
      Relative_PC    : out Address;
      Absolute_PC    : out Address;
      PC_Is_Valid    : out Long_Integer;
      User_Act_Proc  : Address           := Dummy_User_Act_Proc'Address;
      User_Arg_Value : User_Arg_Type     := 0);
   --  Comment on above procedure required ???

   pragma Import (External, Symbolize);

   pragma Import_Valued_Procedure
     (Symbolize, "TBK$SYMBOLIZE",
      (Cond_Value_Type, Address, Address, Address, Address,
       Address, Address, Address, Integer,
       Address, Address, Long_Integer,
       Address, User_Arg_Type),
      (Value, Value, Value, Value, Value,
       Reference, Reference, Reference, Reference,
       Reference, Reference, Reference,
       Value, Value));

   function Decode_Ada_Name (Encoded_Name : String) return String;
   --  Decodes an Ada identifier name. Removes leading "_ada_" and trailing
   --  __{DIGIT}+ or ${DIGIT}+, converts other "__" to '.'

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

   -------------------------
   -- Dummy_User_Act_Proc --
   -------------------------

   function Dummy_User_Act_Proc
     (Msgvec : Address       := Null_Address;
      Actrtn : Address       := Null_Address;
      Facnam : Address       := Null_Address;
      Actprm : User_Arg_Type := 0) return Cond_Value_Type
   is
   begin
      return 0;
   end Dummy_User_Act_Proc;

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String is
      Status            : Cond_Value_Type;
      Image_Name        : ASCIC;
      Image_Name_Addr   : Address;
      Module_Name       : ASCIC;
      Module_Name_Addr  : Address;
      Routine_Name      : ASCIC;
      Routine_Name_Addr : Address;
      Line_Number       : Integer;
      Relative_PC       : Address;
      Absolute_PC       : Address;
      PC_Is_Valid       : Long_Integer;
      Return_Address    : Address;
      Res               : String (1 .. 256 * Traceback'Length);
      Len               : Integer;

   begin
      if Traceback'Length > 0 then
         Len := 0;

         --  Since image computation is not thread-safe we need task lockout

         System.Soft_Links.Lock_Task.all;

         for J in Traceback'Range loop
            Return_Address :=
              (if J = Traceback'Last then Address_Zero
                                     else PC_For (Traceback (J + 1)));

            Symbolize
              (Status,
               PC_For (Traceback (J)),
               PC_For (Traceback (J)),
               PV_For (Traceback (J)),
               Return_Address,
               Image_Name_Addr,
               Module_Name_Addr,
               Routine_Name_Addr,
               Line_Number,
               Relative_PC,
               Absolute_PC,
               PC_Is_Valid);

            Image_Name   := Fetch_ASCIC (Image_Name_Addr);
            Module_Name  := Fetch_ASCIC (Module_Name_Addr);
            Routine_Name := Fetch_ASCIC (Routine_Name_Addr);

            declare
               First : Integer := Len + 1;
               Last  : Integer := First + 80 - 1;
               Pos   : Integer;
               Routine_Name_D : String := Decode_Ada_Name
                 (To_Ada
                    (Routine_Name.Data (1 .. size_t (Routine_Name.Count)),
                     False));

            begin
               Res (First .. Last) := (others => ' ');

               Res (First .. First + Integer (Image_Name.Count) - 1) :=
                 To_Ada
                  (Image_Name.Data (1 .. size_t (Image_Name.Count)),
                   False);

               Res (First + 10 ..
                    First + 10 + Integer (Module_Name.Count) - 1) :=
                 To_Ada
                  (Module_Name.Data (1 .. size_t (Module_Name.Count)),
                   False);

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

               Res (Pos .. Pos + Integer'Image (Line_Number)'Length - 1) :=
                 Integer'Image (Line_Number);

               Res (Last) := ASCII.LF;
               Len := Last;
            end;
         end loop;

         System.Soft_Links.Unlock_Task.all;
         return Res (1 .. Len);

      else
         return "";
      end if;
   end Symbolic_Traceback;

   function Symbolic_Traceback (E : Exception_Occurrence) return String is
   begin
      return Symbolic_Traceback (Tracebacks (E));
   end Symbolic_Traceback;

end GNAT.Traceback.Symbolic;
