------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1999-2003 Ada Core Technologies, Inc.            --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  Run-time symbolic traceback support for VMS

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.Aux_DEC;
with System.Soft_Links;
with System.Traceback_Entries;

package body GNAT.Traceback.Symbolic is

   pragma Warnings (Off);
   pragma Linker_Options ("--for-linker=sys$library:trace.exe");

   use Interfaces.C.Strings;
   use System;
   use System.Aux_DEC;
   use System.Traceback_Entries;

   type Dscdef1_Type is record
      Maxstrlen : Unsigned_Word;
      Dtype     : Unsigned_Byte;
      Class     : Unsigned_Byte;
      Pointer   : chars_ptr;
   end record;

   for Dscdef1_Type use record
      Maxstrlen at 0 range 0 .. 15;
      Dtype     at 2 range 0 .. 7;
      Class     at 3 range 0 .. 7;
      Pointer   at 4 range 0 .. 31;
   end record;
   for Dscdef1_Type'Size use 64;

   Image_Buf  : String (1 .. 10240);
   Image_Len  : Integer;
   Image_Need_Hdr : Boolean := True;
   Image_Do_Another_Line : Boolean;
   Image_Xtra_Msg : Boolean;

   procedure Traceback_Image (Out_Desc : access Dscdef1_Type);

   procedure Traceback_Image (Out_Desc : access Dscdef1_Type) is
      Image : String (1 .. Integer (Out_Desc.Maxstrlen));
   begin
      Image := Value (Out_Desc.Pointer,
                      Interfaces.C.size_t (Out_Desc.Maxstrlen));

      if Image_Do_Another_Line and then
        (Image_Need_Hdr or else
         Image (Image'First .. Image'First + 27) /=
         "  image    module    routine")
      then
         declare
            First : Integer := Image_Len + 1;
            Last  : Integer := First + Image'Length - 1;
         begin
            Image_Buf (First .. Last + 1) := Image & ASCII.LF;
            Image_Len := Last + 1;
         end;

         Image_Need_Hdr := False;

         if Image (Image'First .. Image'First + 3) = "----" then
            if Image_Xtra_Msg = False then
               Image_Xtra_Msg := True;
            else
               Image_Xtra_Msg := False;
            end if;
         end if;

         if Out_Desc.Maxstrlen = 79 and then not Image_Xtra_Msg then
            Image_Len := Image_Len - 1;
            Image_Do_Another_Line := False;
         end if;
      end if;
   end Traceback_Image;

   subtype User_Arg_Type is Unsigned_Longword;
   subtype Cond_Value_Type is Unsigned_Longword;

   procedure Show_Traceback
     (Status         : out Cond_Value_Type;
      Faulting_FP    : Address;
      Faulting_SP    : Address;
      Faulting_PC    : Address;
      Detail_Level   : Integer           := Integer'Null_Parameter;
      User_Act_Proc  : Address           := Address'Null_Parameter;
      User_Arg_Value : User_Arg_Type     := User_Arg_Type'Null_Parameter;
      Exceptionn     : Unsigned_Longword := Unsigned_Longword'Null_Parameter);

   pragma Interface (External, Show_Traceback);

   pragma Import_Valued_Procedure
     (Show_Traceback, "TBK$SHOW_TRACEBACK",
      (Cond_Value_Type, Address, Address, Address, Integer, Address,
       User_Arg_Type, Unsigned_Longword),
      (Value, Value, Value, Value, Reference, Value, Value, Reference),
       Detail_Level);


   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String is
      Res : String (1 .. 256 * Traceback'Length);
      Len : Integer;
      Status : Cond_Value_Type;

   begin
      if Traceback'Length > 0 then

         Len := 0;

         --  Since image computation is not thread-safe we need task lockout
         System.Soft_Links.Lock_Task.all;
         for I in Traceback'Range loop
            Image_Len := 0;
            Image_Do_Another_Line := True;
            Image_Xtra_Msg := False;

            Show_Traceback
              (Status,
               FP_For (Traceback (I)),
               SP_For (Traceback (I)),
               PC_For (Traceback (I)),
               0,
               Traceback_Image'Address);

            declare
               First : Integer := Len + 1;
               Last  : Integer := First + Image_Len - 1;
            begin
               Res (First .. Last + 1) := Image_Buf & ASCII.LF;
               Len := Last + 1;
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
