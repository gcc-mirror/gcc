------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     SYSTEM.MACHINE_STATE_OPERATIONS                      --
--                                                                          --
--                                 B o d y                                  --
--                         (Version for Alpha/VMS)                          --
--                                                                          --
--                     Copyright (C) 2001-2007, AdaCore                     --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This version of System.Machine_State_Operations is for use on
--  Alpha systems running VMS.

with System.Memory;
with System.Aux_DEC; use System.Aux_DEC;
with Ada.Unchecked_Conversion;

package body System.Machine_State_Operations is

   subtype Cond_Value_Type is Unsigned_Longword;

   --  Record layouts copied from Starlet

   type ICB_Fflags_Bits_Type is record
      Exception_Frame : Boolean;
      Ast_Frame       : Boolean;
      Bottom_Of_Stack : Boolean;
      Base_Frame      : Boolean;
      Filler_1        : Unsigned_20;
   end record;

   for ICB_Fflags_Bits_Type use record
      Exception_Frame at 0 range 0 .. 0;
      Ast_Frame       at 0 range 1 .. 1;
      Bottom_Of_Stack at 0 range 2 .. 2;
      Base_Frame      at 0 range 3 .. 3;
      Filler_1        at 0 range 4 .. 23;
   end record;
   for ICB_Fflags_Bits_Type'Size use 24;

   type ICB_Hdr_Quad_Type is record
      Context_Length : Unsigned_Longword;
      Fflags_Bits    : ICB_Fflags_Bits_Type;
      Block_Version  : Unsigned_Byte;
   end record;

   for ICB_Hdr_Quad_Type use record
      Context_Length at 0 range 0 .. 31;
      Fflags_Bits    at 4 range 0 .. 23;
      Block_Version  at 7 range 0 .. 7;
   end record;
   for ICB_Hdr_Quad_Type'Size use 64;

   type Invo_Context_Blk_Type is record

      Hdr_Quad : ICB_Hdr_Quad_Type;
      --  The first quadword contains:
      --    o  The length of the structure in bytes (a longword field)
      --    o  The frame flags (a 3 byte field of bits)
      --    o  The version number (a 1 byte field)

      Procedure_Descriptor : Unsigned_Quadword;
      --  The address of the procedure descriptor for the procedure

      Program_Counter : Integer_64;
      --  The current PC of a given procedure invocation

      Processor_Status : Integer_64;
      --  The current PS of a given procedure invocation

      Ireg : Unsigned_Quadword_Array (0 .. 30);
      Freg : Unsigned_Quadword_Array (0 .. 30);
      --  The register contents areas. 31 for scalars, 31 for float

      System_Defined : Unsigned_Quadword_Array (0 .. 1);
      --  The following is an "internal" area that's reserved for use by
      --  the operating system. It's size may vary over time.

      --  Chfctx_Addr : Unsigned_Quadword;
      --  Defined as a comment since it overlaps other fields

      Filler_1             : String (1 .. 0);
      --  Align to octaword
   end record;

   for Invo_Context_Blk_Type use record
      Hdr_Quad             at   0 range 0 .. 63;
      Procedure_Descriptor at   8 range 0 .. 63;
      Program_Counter      at  16 range 0 .. 63;
      Processor_Status     at  24 range 0 .. 63;
      Ireg                 at  32 range 0 .. 1983;
      Freg                 at 280 range 0 .. 1983;
      System_Defined       at 528 range 0 .. 127;

      --  Component representation spec(s) below are defined as
      --  comments since they overlap other fields

      --  Chfctx_Addr at 528 range 0 .. 63;

      Filler_1 at 544 range 0 .. -1;
   end record;
   for Invo_Context_Blk_Type'Size use 4352;

   subtype Invo_Handle_Type is Unsigned_Longword;

   type Invo_Handle_Access_Type is access all Invo_Handle_Type;

   function Fetch is new Fetch_From_Address (Code_Loc);

   function To_Invo_Handle_Access is new Ada.Unchecked_Conversion
     (Machine_State, Invo_Handle_Access_Type);

   function To_Machine_State is new Ada.Unchecked_Conversion
     (System.Address, Machine_State);

   ----------------------------
   -- Allocate_Machine_State --
   ----------------------------

   function Allocate_Machine_State return Machine_State is
   begin
      return To_Machine_State
        (Memory.Alloc (Invo_Handle_Type'Max_Size_In_Storage_Elements));
   end Allocate_Machine_State;

   ----------------
   -- Fetch_Code --
   ----------------

   function Fetch_Code (Loc : Code_Loc) return Code_Loc is
   begin
      --  The starting address is in the second longword pointed to by Loc

      return Fetch (System.Aux_DEC."+" (Loc, 8));
   end Fetch_Code;

   ------------------------
   -- Free_Machine_State --
   ------------------------

   procedure Free_Machine_State (M : in out Machine_State) is
   begin
      Memory.Free (Address (M));
      M := Machine_State (Null_Address);
   end Free_Machine_State;

   ------------------
   -- Get_Code_Loc --
   ------------------

   function Get_Code_Loc (M : Machine_State) return Code_Loc is
      procedure Get_Invo_Context (
         Result       : out Unsigned_Longword; -- return value
         Invo_Handle  : Invo_Handle_Type;
         Invo_Context : out Invo_Context_Blk_Type);

      pragma Interface (External, Get_Invo_Context);

      pragma Import_Valued_Procedure (Get_Invo_Context, "LIB$GET_INVO_CONTEXT",
         (Unsigned_Longword, Invo_Handle_Type, Invo_Context_Blk_Type),
         (Value, Value, Reference));

      Asm_Call_Size : constant := 4;
      --  Under VMS a call
      --  asm instruction takes 4 bytes. So we must remove this amount.

      ICB : Invo_Context_Blk_Type;
      Status : Cond_Value_Type;

   begin
      Get_Invo_Context (Status, To_Invo_Handle_Access (M).all, ICB);

      if (Status and 1) /= 1 then
         return Code_Loc (System.Null_Address);
      end if;

      return Code_Loc (ICB.Program_Counter - Asm_Call_Size);
   end Get_Code_Loc;

   --------------------------
   -- Machine_State_Length --
   --------------------------

   function Machine_State_Length
     return System.Storage_Elements.Storage_Offset
   is
      use System.Storage_Elements;

   begin
      return Invo_Handle_Type'Size / 8;
   end Machine_State_Length;

   ---------------
   -- Pop_Frame --
   ---------------

   procedure Pop_Frame (M : Machine_State) is
      procedure Get_Prev_Invo_Handle (
         Result : out Invo_Handle_Type; -- return value
         ICB    : Invo_Handle_Type);

      pragma Interface (External, Get_Prev_Invo_Handle);

      pragma Import_Valued_Procedure
        (Get_Prev_Invo_Handle, "LIB$GET_PREV_INVO_HANDLE",
         (Invo_Handle_Type, Invo_Handle_Type),
         (Value, Value));

      Prev_Handle : aliased Invo_Handle_Type;

   begin
      Get_Prev_Invo_Handle (Prev_Handle, To_Invo_Handle_Access (M).all);
      To_Invo_Handle_Access (M).all := Prev_Handle;
   end Pop_Frame;

   -----------------------
   -- Set_Machine_State --
   -----------------------

   procedure Set_Machine_State (M : Machine_State) is

      procedure Get_Curr_Invo_Context
        (Invo_Context : out Invo_Context_Blk_Type);

      pragma Interface (External, Get_Curr_Invo_Context);

      pragma Import_Valued_Procedure
        (Get_Curr_Invo_Context, "LIB$GET_CURR_INVO_CONTEXT",
         (Invo_Context_Blk_Type),
         (Reference));

      procedure Get_Invo_Handle (
         Result       : out Invo_Handle_Type; -- return value
         Invo_Context : Invo_Context_Blk_Type);

      pragma Interface (External, Get_Invo_Handle);

      pragma Import_Valued_Procedure (Get_Invo_Handle, "LIB$GET_INVO_HANDLE",
         (Invo_Handle_Type, Invo_Context_Blk_Type),
         (Value, Reference));

      ICB         : Invo_Context_Blk_Type;
      Invo_Handle : aliased Invo_Handle_Type;

   begin
      Get_Curr_Invo_Context (ICB);
      Get_Invo_Handle (Invo_Handle, ICB);
      To_Invo_Handle_Access (M).all := Invo_Handle;
      Pop_Frame (M, System.Null_Address);
   end Set_Machine_State;

end System.Machine_State_Operations;
