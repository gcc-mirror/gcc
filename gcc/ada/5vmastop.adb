------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     SYSTEM.MACHINE_STATE_OPERATIONS                      --
--                                                                          --
--                                 B o d y                                  --
--                         (Version for Alpha/VMS)                          --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--              Copyright (C) 2001 Ada Core Technologies, Inc.              --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This version of System.Machine_State_Operations is for use on
--  Alpha systems running VMS.

with System.Memory;
with System.Aux_DEC; use System.Aux_DEC;
with Unchecked_Conversion;

package body System.Machine_State_Operations is

   use System.Exceptions;
   subtype Cond_Value_Type is Unsigned_Longword;

   --  Record layouts copied from Starlet.

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

   ICB_Fflags_Bits_Type_Init : constant ICB_Fflags_Bits_Type :=
     (ExceptIon_Frame    => False,
      Ast_Frame          => False,
      Bottom_Of_STACK    => False,
      Base_Frame         => False,
      Filler_1           => 0);

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

   ICB_Hdr_Quad_Type_Init : constant ICB_Hdr_Quad_Type :=
     (Context_Length => 0,
      Fflags_Bits    => ICB_Fflags_Bits_Type_Init,
      Block_Version  => 0);

   type Invo_Context_Blk_Type is record
      --
      --  The first quadword contains:
      --      o  The length of the structure in bytes (a longword field)
      --      o  The frame flags (a 3 byte field of bits)
      --      o  The version number (a 1 byte field)
      --
      Hdr_Quad             : ICB_Hdr_Quad_Type;
      --
      --  The address of the procedure descriptor for the procedure.
      --
      Procedure_Descriptor : Unsigned_Quadword;
      --
      --  The current PC of a given procedure invocation.
      --
      Program_Counter      : Integer_64;
      --
      --  The current PS of a given procedure invocation.
      --
      Processor_Status     : Integer_64;
      --
      --  The register contents areas. 31 for scalars, 31 for float.
      --
      Ireg                 : Unsigned_Quadword_Array (0 .. 30);
      Freg                 : Unsigned_Quadword_Array (0 .. 30);
      --
      --  The following is an "internal" area that's reserved for use by
      --  the operating system. It's size may vary over time.
      --
      System_Defined       : Unsigned_Quadword_Array (0 .. 1);

      ----Component(s) below are defined as comments since they
      ----overlap other fields
      ----
      ----Chfctx_Addr      : Unsigned_Quadword;

      --
      --  Align to octaword.
      --
      Filler_1             : String (1 .. 0);
   end record;

   for Invo_Context_Blk_Type use record
      Hdr_Quad             at   0 range 0 .. 63;
      Procedure_Descriptor at   8 range 0 .. 63;
      Program_Counter      at  16 range 0 .. 63;
      Processor_Status     at  24 range 0 .. 63;
      Ireg                 at  32 range 0 .. 1983;
      Freg                 at 280 range 0 .. 1983;
      System_Defined       at 528 range 0 .. 127;

      ----Component representation spec(s) below are defined as
      ----comments since they overlap other fields
      ----
      ----Chfctx_Addr at 528 range 0 .. 63;

      Filler_1 at 544 range 0 .. -1;
   end record;
   for Invo_Context_Blk_Type'Size use 4352;

   Invo_Context_Blk_Type_Init : constant Invo_Context_Blk_Type :=
     (Hdr_Quad             => ICB_Hdr_Quad_Type_Init,
      Procedure_Descriptor => (0, 0),
      Program_Counter      => 0,
      Processor_Status     => 0,
      Ireg                 => (others => (0, 0)),
      Freg                 => (others => (0, 0)),
      System_Defined       => (others => (0, 0)),
      Filler_1             => (others => ASCII.NUL));

   subtype Invo_Handle_Type is Unsigned_Longword;

   type Invo_Handle_Access_Type is access all Invo_Handle_Type;

   function Fetch is new Fetch_From_Address (Code_Loc);

   function To_Invo_Handle_Access is new Unchecked_Conversion
     (Machine_State, Invo_Handle_Access_Type);

   function To_Machine_State is new Unchecked_Conversion
     (System.Address, Machine_State);

   function To_Code_Loc is new Unchecked_Conversion
     (Unsigned_Longword, Code_Loc);

   ----------------------------
   -- Allocate_Machine_State --
   ----------------------------

   function Allocate_Machine_State return Machine_State is
   begin
      return To_Machine_State
        (Memory.Alloc (Invo_Handle_Type'Max_Size_In_Storage_Elements));
   end Allocate_Machine_State;

   -------------------
   -- Enter_Handler --
   -------------------

   procedure Enter_Handler (M : Machine_State; Handler : Handler_Loc) is
      procedure Get_Invo_Context (
         Result       : out Unsigned_Longword; -- return value
         Invo_Handle  : in  Invo_Handle_Type;
         Invo_Context : out Invo_Context_Blk_Type);

      pragma Interface (External, Get_Invo_Context);

      pragma Import_Valued_Procedure (Get_Invo_Context, "LIB$GET_INVO_CONTEXT",
         (Unsigned_Longword, Invo_Handle_Type, Invo_Context_Blk_Type),
         (Value, Value, Reference));

      ICB : Invo_Context_Blk_Type;

      procedure Goto_Unwind (
         Status      : out Cond_Value_Type; -- return value
         Target_Invo : in  Address := Address_Zero;
         Target_PC   : in  Address := Address_Zero;
         New_R0      : in  Unsigned_Quadword
          := Unsigned_Quadword'Null_Parameter;
         New_R1      : in  Unsigned_Quadword
          := Unsigned_Quadword'Null_Parameter);

      pragma Interface (External, Goto_Unwind);

      pragma Import_Valued_Procedure
        (Goto_Unwind, "SYS$GOTO_UNWIND",
         (Cond_Value_Type, Address, Address,
          Unsigned_Quadword, Unsigned_Quadword),
         (Value, Reference, Reference,
          Reference, Reference));

      Status   : Cond_Value_Type;

   begin
      Get_Invo_Context (Status, To_Invo_Handle_Access (M).all, ICB);
      Goto_Unwind
        (Status, System.Address (To_Invo_Handle_Access (M).all), Handler);
   end Enter_Handler;

   ----------------
   -- Fetch_Code --
   ----------------

   function Fetch_Code (Loc : Code_Loc) return Code_Loc is
   begin
      --  The starting address is in the second longword pointed to by Loc.
      return Fetch (System.Aux_DEC."+" (Loc, 8));
   end Fetch_Code;

   ------------------------
   -- Free_Machine_State --
   ------------------------

   procedure Free_Machine_State (M : in out Machine_State) is
      procedure Gnat_Free (M : in Invo_Handle_Access_Type);
      pragma Import (C, Gnat_Free, "__gnat_free");

   begin
      Gnat_Free (To_Invo_Handle_Access (M));
      M := Machine_State (Null_Address);
   end Free_Machine_State;

   ------------------
   -- Get_Code_Loc --
   ------------------

   function Get_Code_Loc (M : Machine_State) return Code_Loc is
      procedure Get_Invo_Context (
         Result       : out Unsigned_Longword; -- return value
         Invo_Handle  : in  Invo_Handle_Type;
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

   procedure Pop_Frame
     (M    : Machine_State;
      Info : Subprogram_Info_Type)
   is

      procedure Get_Prev_Invo_Handle (
         Result : out Invo_Handle_Type; -- return value
         ICB    : in  Invo_Handle_Type);

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
         Invo_Context : in Invo_Context_Blk_Type);

      pragma Interface (External, Get_Invo_Handle);

      pragma Import_Valued_Procedure (Get_Invo_Handle, "LIB$GET_INVO_HANDLE",
         (Invo_Handle_Type, Invo_Context_Blk_Type),
         (Value, Reference));

      ICB : Invo_Context_Blk_Type;
      Invo_Handle : aliased Invo_Handle_Type;

   begin
      Get_Curr_Invo_Context (ICB);
      Get_Invo_Handle (Invo_Handle, ICB);
      To_Invo_Handle_Access (M).all := Invo_Handle;
      Pop_Frame (M, System.Null_Address);
   end Set_Machine_State;

   ------------------------------
   -- Set_Signal_Machine_State --
   ------------------------------

   procedure Set_Signal_Machine_State
     (M       : Machine_State;
      Context : System.Address) is
   begin
      null;
   end Set_Signal_Machine_State;

end System.Machine_State_Operations;
