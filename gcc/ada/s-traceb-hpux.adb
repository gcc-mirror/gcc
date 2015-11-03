------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                             (HP/UX Version)                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2009-2015, Free Software Foundation, Inc.        --
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

with Ada.Unchecked_Conversion;

package body System.Traceback is

   --  This package implements the backtracing facility by way of a dedicated
   --  HP library for stack unwinding described in the "Runtime Architecture
   --  Document".

   pragma Linker_Options ("/usr/lib/libcl.a");

   --  The library basically offers services to fetch information about a
   --  "previous" frame based on information about a "current" one.

   type Current_Frame_Descriptor is record
      cur_fsz : Address;  --  Frame size of current routine.
      cur_sp  : Address;  --  The current value of stack pointer.
      cur_rls : Address;  --  PC-space of the caller.
      cur_rlo : Address;  --  PC-offset of the caller.
      cur_dp  : Address;  --  Data Pointer of the current routine.
      top_rp  : Address;  --  Initial value of RP.
      top_mrp : Address;  --  Initial value of MRP.
      top_sr0 : Address;  --  Initial value of sr0.
      top_sr4 : Address;  --  Initial value of sr4.
      top_r3  : Address;  --  Initial value of gr3.
      cur_r19 : Address;  --  GR19 value of the calling routine.
      top_r4  : Address;  --  Initial value of gr4.
      dummy   : Address;  --  Reserved.
      out_rlo : Address;  --  PC-offset of the caller after get_previous.
   end record;

   type Previous_Frame_Descriptor is record
      prev_fsz : Address;  --  frame size of calling routine.
      prev_sp  : Address;  --  SP of calling routine.
      prev_rls : Address;  --  PC_space of calling routine's caller.
      prev_rlo : Address;  --  PC_offset of calling routine's caller.
      prev_dp  : Address;  --  DP of calling routine.
      udescr0  : Address;  --  low word of calling routine's unwind desc.
      udescr1  : Address;  --  high word of calling routine's unwind desc.
      ustart   : Address;  --  start of the unwind region.
      uend     : Address;  --  end of the unwind region.
      uw_index : Address;  --  index into the unwind table.
      prev_r19 : Address;  --  GR19 value of the caller's caller.
      top_r3   : Address;  --  Caller's initial gr3.
      top_r4   : Address;  --  Caller's initial gr4.
   end record;

   --  Provide useful shortcuts for the names

   subtype CFD is Current_Frame_Descriptor;
   subtype PFD is Previous_Frame_Descriptor;

   --  Frames with dynamic stack allocation are handled using the associated
   --  frame pointer, but HP compilers and GCC setup this pointer differently.
   --  HP compilers set it to point at the top (highest address) of the static
   --  part of the frame, whereas GCC sets it to point at the bottom of this
   --  region. We have to fake the unwinder to compensate for this difference,
   --  for which we'll need to access some subprograms unwind descriptors.

   type Bits_2_Value is mod 2 ** 2;
   for Bits_2_Value'Size use 2;

   type Bits_4_Value  is mod 2 ** 4;
   for Bits_4_Value'Size use 4;

   type Bits_5_Value  is mod 2 ** 5;
   for Bits_5_Value'Size use 5;

   type Bits_27_Value is mod 2 ** 27;
   for Bits_27_Value'Size use 27;

   type Unwind_Descriptor is record
      cannot_unwind         : Boolean;
      mcode                 : Boolean;
      mcode_save_restore    : Boolean;
      region_desc           : Bits_2_Value;
      reserved0             : Boolean;
      entry_sr              : Boolean;
      entry_fr              : Bits_4_Value;
      entry_gr              : Bits_5_Value;

      args_stored           : Boolean;
      variable_frame        : Boolean;
      separate_package_body : Boolean;
      frame_extension_mcode : Boolean;

      stack_overflow_check  : Boolean;
      two_steps_sp_adjust   : Boolean;
      sr4_export            : Boolean;
      cxx_info              : Boolean;

      cxx_try_catch         : Boolean;
      sched_entry_seq       : Boolean;
      reserved1             : Boolean;
      save_sp               : Boolean;

      save_rp               : Boolean;
      save_mrp              : Boolean;
      save_r19              : Boolean;
      cleanups              : Boolean;

      hpe_interrupt_marker  : Boolean;
      hpux_interrupt_marker : Boolean;
      large_frame           : Boolean;
      alloca_frame          : Boolean;

      reserved2             : Boolean;
      frame_size            : Bits_27_Value;
   end record;

   for Unwind_Descriptor'Size use 64;

   for Unwind_Descriptor use record
      cannot_unwind         at 0 range 0 .. 0;
      mcode                 at 0 range 1 .. 1;
      mcode_save_restore    at 0 range 2 .. 2;
      region_desc           at 0 range 3 .. 4;
      reserved0             at 0 range 5 .. 5;
      entry_sr              at 0 range 6 .. 6;
      entry_fr              at 0 range 7 .. 10;

      entry_gr              at 1 range 3 .. 7;

      args_stored           at 2 range 0 .. 0;
      variable_frame        at 2 range 1 .. 1;
      separate_package_body at 2 range 2 .. 2;
      frame_extension_mcode at 2 range 3 .. 3;
      stack_overflow_check  at 2 range 4 .. 4;
      two_steps_sp_adjust   at 2 range 5 .. 5;
      sr4_export            at 2 range 6 .. 6;
      cxx_info              at 2 range 7 .. 7;

      cxx_try_catch         at 3 range 0 .. 0;
      sched_entry_seq       at 3 range 1 .. 1;
      reserved1             at 3 range 2 .. 2;
      save_sp               at 3 range 3 .. 3;
      save_rp               at 3 range 4 .. 4;
      save_mrp              at 3 range 5 .. 5;
      save_r19              at 3 range 6 .. 6;
      cleanups              at 3 range 7 .. 7;

      hpe_interrupt_marker  at 4 range 0 .. 0;
      hpux_interrupt_marker at 4 range 1 .. 1;
      large_frame           at 4 range 2 .. 2;
      alloca_frame          at 4 range 3 .. 3;

      reserved2             at 4 range 4 .. 4;
      frame_size            at 4 range 5 .. 31;
   end record;

   subtype UWD is Unwind_Descriptor;
   type UWD_Ptr is access all UWD;

   function To_UWD_Access is new Ada.Unchecked_Conversion (Address, UWD_Ptr);

   --  The descriptor associated with a given code location is retrieved
   --  using functions imported from the HP library, requiring the definition
   --  of additional structures.

   type Unwind_Table_Region is record
      Table_Start : Address;
      Table_End   : Address;
   end record;
   --  An Unwind Table region, which is a memory area containing Unwind
   --  Descriptors.

   subtype UWT is Unwind_Table_Region;

   --  The subprograms imported below are provided by the HP library

   function U_get_unwind_table return UWT;
   pragma Import (C, U_get_unwind_table, "U_get_unwind_table");
   --  Get the unwind table region associated with the current executable.
   --  This function is actually documented as having an argument, but which
   --  is only used for the MPE/iX targets.

   function U_get_shLib_unwind_table (r19 : Address) return UWT;
   pragma Import (C, U_get_shLib_unwind_table, "U_get_shLib_unw_tbl");
   --  Return the unwind table region associated with a possible shared
   --  library, as determined by the provided r19 value.

   function U_get_shLib_text_addr (r19 : Address) return Address;
   pragma Import (C, U_get_shLib_text_addr, "U_get_shLib_text_addr");
   --  Return the address at which the code for a shared library begins, or
   --  -1 if the value provided for r19 does not identify shared library code.

   function U_get_unwind_entry
     (Pc          : Address;
      Space       : Address;
      Table_Start : Address;
      Table_End   : Address) return Address;
   pragma Import (C, U_get_unwind_entry, "U_get_unwind_entry");
   --  Given the bounds of an unwind table, return the address of the
   --  unwind descriptor associated with a code location/space. In the case
   --  of shared library code, the offset from the beginning of the library
   --  is expected as Pc.

   procedure U_init_frame_record (Frame : not null access CFD);
   pragma Import (C, U_init_frame_record, "U_init_frame_record");

   procedure U_prep_frame_rec_for_unwind (Frame : not null access CFD);
   pragma Import (C, U_prep_frame_rec_for_unwind,
                    "U_prep_frame_rec_for_unwind");

   --  Fetch the description data of the frame in which these two procedures
   --  are called.

   function U_get_u_rlo
     (Cur : not null access CFD; Prev : not null access PFD) return Integer;
   pragma Import (C, U_get_u_rlo, "U_IS_STUB_OR_CALLX");
   --  From a complete current frame with a return location possibly located
   --  into a linker generated stub, and basic information about the previous
   --  frame, place the first non stub return location into the current frame.
   --  Return -1 if something went wrong during the computation.

   function U_is_shared_pc (rlo : Address; r19 : Address) return Address;
   pragma Import (C, U_is_shared_pc, "U_is_shared_pc");
   --  Return 0 if the provided return location does not correspond to code
   --  in a shared library, or something non null otherwise.

   function U_get_previous_frame_x
     (current_frame  : not null access CFD;
      previous_frame : not null access PFD;
      previous_size  : Integer) return Integer;
   pragma Import (C, U_get_previous_frame_x, "U_get_previous_frame_x");
   --  Fetch the data describing the "previous" frame relatively to the
   --  "current" one. "previous_size" should be the size of the "previous"
   --  frame descriptor provided.
   --
   --  The library provides a simpler interface without the size parameter
   --  but it is not usable when frames with dynamically allocated space are
   --  on the way.

   procedure Call_Chain
     (Traceback   : System.Address;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1);
   --  Same as the exported version, but takes Traceback as an Address

   ------------------
   -- C_Call_Chain --
   ------------------

   function C_Call_Chain
     (Traceback : System.Address;
      Max_Len   : Natural) return Natural
   is
      Val : Natural;
   begin
      Call_Chain (Traceback, Max_Len, Val);
      return Val;
   end C_Call_Chain;

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain
     (Traceback   : System.Address;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
      type Tracebacks_Array is array (1 .. Max_Len) of System.Address;
      pragma Suppress_Initialization (Tracebacks_Array);

      --  The code location returned by the unwinder is a return location but
      --  what we need is a call point. Under HP-UX call instructions are 4
      --  bytes long and the return point they specify is 4 bytes beyond the
      --  next instruction because of the delay slot.

      Call_Size  : constant := 4;
      DSlot_Size : constant := 4;
      Rlo_Offset : constant := Call_Size + DSlot_Size;

      --  Moreover, the return point is passed via a register which two least
      --  significant bits specify a privilege level that we will have to mask.

      Priv_Mask  : constant := 16#00000003#;

      Frame       : aliased CFD;
      Code        : System.Address;
      J           : Natural := 1;
      Pop_Success : Boolean;
      Trace       : Tracebacks_Array;
      for Trace'Address use Traceback;

      --  The backtracing process needs a set of subprograms :

      function UWD_For_RLO_Of (Frame : not null access CFD) return UWD_Ptr;
      --  Return an access to the unwind descriptor for the caller of
      --  a given frame, using only the provided return location.

      function UWD_For_Caller_Of (Frame : not null access CFD) return UWD_Ptr;
      --  Return an access to the unwind descriptor for the user code caller
      --  of a given frame, or null if the information is not available.

      function Pop_Frame (Frame : not null access CFD) return Boolean;
      --  Update the provided machine state structure so that it reflects
      --  the state one call frame "above" the initial one.
      --
      --  Return True if the operation has been successful, False otherwise.
      --  Failure typically occurs when the top of the call stack has been
      --  reached.

      function Prepare_For_Unwind_Of
        (Frame : not null access CFD) return Boolean;
      --  Perform the necessary adaptations to the machine state before
      --  calling the unwinder. Currently used for the specific case of
      --  dynamically sized previous frames.
      --
      --  Return True if everything went fine, or False otherwise.

      Program_UWT : constant UWT := U_get_unwind_table;

      ---------------
      -- Pop_Frame --
      ---------------

      function Pop_Frame (Frame : not null access CFD) return Boolean is
         Up_Frame    : aliased PFD;
         State_Ready : Boolean;

      begin
         --  Check/adapt the state before calling the unwinder and return
         --  if anything went wrong.

         State_Ready := Prepare_For_Unwind_Of (Frame);

         if not State_Ready then
            return False;
         end if;

         --  Now, safely call the unwinder and use the results

         if U_get_previous_frame_x (Frame,
                                    Up_Frame'Access,
                                    Up_Frame'Size) /= 0
         then
            return False;
         end if;

         --  In case a stub is on the way, the usual previous return location
         --  (the one in prev_rlo) is the one in the stub and the "real" one
         --  is placed in the "current" record, so let's take this one into
         --  account.

         Frame.out_rlo := Frame.cur_rlo;

         Frame.cur_fsz := Up_Frame.prev_fsz;
         Frame.cur_sp  := Up_Frame.prev_sp;
         Frame.cur_rls := Up_Frame.prev_rls;
         Frame.cur_rlo := Up_Frame.prev_rlo;
         Frame.cur_dp  := Up_Frame.prev_dp;
         Frame.cur_r19 := Up_Frame.prev_r19;
         Frame.top_r3  := Up_Frame.top_r3;
         Frame.top_r4  := Up_Frame.top_r4;

         return True;
      end Pop_Frame;

      ---------------------------------
      -- Prepare_State_For_Unwind_Of --
      ---------------------------------

      function Prepare_For_Unwind_Of
        (Frame : not null access CFD) return Boolean
      is
         Caller_UWD    : UWD_Ptr;
         FP_Adjustment : Integer;

      begin
         --  No need to bother doing anything if the stack is already fully
         --  unwound.

         if Frame.cur_rlo = 0 then
            return False;
         end if;

         --  When ALLOCA_FRAME is set in an unwind descriptor, the unwinder
         --  uses the value provided in current.top_r3 or current.top_r4 as
         --  a frame pointer to compute the size of the frame. What decides
         --  between r3 or r4 is the unwind descriptor LARGE_FRAME bit, with
         --  r4 chosen if the bit is set.

         --  The size computed by the unwinder is STATIC_PART + (SP - FP),
         --  which is correct with HP's frame pointer convention, but not
         --  with GCC's one since we end up with the static part accounted
         --  for twice.

         --  We have to compute r4 when it is required because the unwinder
         --  has looked for it at a place where it was not if we went through
         --  GCC frames.

         --  The size of the static part of a frame can be found in the
         --  associated unwind descriptor.

         Caller_UWD := UWD_For_Caller_Of (Frame);

         --  If we cannot get it, we are unable to compute the potentially
         --  necessary adjustments. We'd better not try to go on then.

         if Caller_UWD = null then
            return False;
         end if;

         --  If the caller frame is a GCC one, r3 is its frame pointer and
         --  points to the bottom of the frame. The value to provide for r4
         --  can then be computed directly from the one of r3, compensating
         --  for the static part of the frame.

         --  If the caller frame is an HP one, r3 is used to locate the
         --  previous frame marker, that is it also points to the bottom of
         --  the frame (this is why r3 cannot be used as the frame pointer in
         --  the HP sense for large frames). The value to provide for r4 can
         --  then also be computed from the one of r3 with the compensation
         --  for the static part of the frame.

         FP_Adjustment := Integer (Caller_UWD.frame_size * 8);
         Frame.top_r4  := Address (Integer (Frame.top_r3) + FP_Adjustment);

         return True;
      end Prepare_For_Unwind_Of;

      -----------------------
      -- UWD_For_Caller_Of --
      -----------------------

      function UWD_For_Caller_Of (Frame : not null access CFD) return UWD_Ptr
      is
         UWD_Access : UWD_Ptr;

      begin
         --  First try the most direct path, using the return location data
         --  associated with the frame.

         UWD_Access := UWD_For_RLO_Of (Frame);

         if UWD_Access /= null then
            return UWD_Access;
         end if;

         --  If we did not get a result, we might face an in-stub return
         --  address. In this case U_get_previous_frame can tell us what the
         --  first not-in-stub return point is. We cannot call it directly,
         --  though, because we haven't computed the potentially necessary
         --  frame pointer adjustments, which might lead to SEGV in some
         --  circumstances. Instead, we directly call the libcl routine which
         --  is called by U_get_previous_frame and which only requires few
         --  information. Take care, however, that the information is provided
         --  in the "current" argument, so we need to work on a copy to avoid
         --  disturbing our caller.

         declare
            U_Current  : aliased CFD := Frame.all;
            U_Previous : aliased PFD;

         begin
            U_Previous.prev_dp  := U_Current.cur_dp;
            U_Previous.prev_rls := U_Current.cur_rls;
            U_Previous.prev_sp  := U_Current.cur_sp - U_Current.cur_fsz;

            if U_get_u_rlo (U_Current'Access, U_Previous'Access) /= -1 then
               UWD_Access := UWD_For_RLO_Of (U_Current'Access);
            end if;
         end;

         return UWD_Access;
      end UWD_For_Caller_Of;

      --------------------
      -- UWD_For_RLO_Of --
      --------------------

      function UWD_For_RLO_Of (Frame : not null access CFD) return UWD_Ptr
      is
         UWD_Address : Address;

         --  The addresses returned by the library point to full descriptors
         --  including the frame information bits but also the applicable PC
         --  range. We need to account for this.

         Frame_Info_Offset  : constant := 8;

      begin
         --  First try to locate the descriptor in the program's unwind table

         UWD_Address := U_get_unwind_entry (Frame.cur_rlo,
                                            Frame.cur_rls,
                                            Program_UWT.Table_Start,
                                            Program_UWT.Table_End);

         --  If we did not get it, we might have a frame from code in a
         --  stub or shared library. For code in stub we would have to
         --  compute the first non-stub return location but this is not
         --  the role of this subprogram, so let's just try to see if we
         --  can get a result from the tables in shared libraries.

         if UWD_Address = -1
           and then U_is_shared_pc (Frame.cur_rlo, Frame.cur_r19) /= 0
         then
            declare
               Shlib_UWT   : constant UWT     :=
                               U_get_shLib_unwind_table (Frame.cur_r19);
               Shlib_Start : constant Address :=
                               U_get_shLib_text_addr (Frame.cur_r19);
               Rlo_Offset  : constant Address :=
                               Frame.cur_rlo - Shlib_Start;
            begin
               UWD_Address := U_get_unwind_entry (Rlo_Offset,
                                                  Frame.cur_rls,
                                                  Shlib_UWT.Table_Start,
                                                  Shlib_UWT.Table_End);
            end;
         end if;

         if UWD_Address /= -1 then
            return To_UWD_Access (UWD_Address + Frame_Info_Offset);
         else
            return null;
         end if;
      end UWD_For_RLO_Of;

   --  Start of processing for Call_Chain

   begin
      --  Fetch the state for this subprogram's frame and pop it so that we
      --  start with an initial out_rlo "here".

      U_init_frame_record (Frame'Access);
      Frame.top_sr0 := 0;
      Frame.top_sr4 := 0;

      U_prep_frame_rec_for_unwind (Frame'Access);

      Pop_Success := Pop_Frame (Frame'Access);

      --  Skip the requested number of frames

      for I in 1 .. Skip_Frames loop
         Pop_Success := Pop_Frame (Frame'Access);
      end loop;

      --  Loop popping frames and storing locations until either a problem
      --  occurs, or the top of the call chain is reached, or the provided
      --  array is full.

      loop
         --  We have to test some conditions against the return location
         --  as it is returned, so get it as is first.

         Code := Frame.out_rlo;

         exit when not Pop_Success or else Code = 0 or else J = Max_Len + 1;

         --  Compute the call point from the retrieved return location :
         --  Mask the privilege bits and account for the delta between the
         --  call site and the return point.

         Code := (Code and not Priv_Mask) - Rlo_Offset;

         if Code < Exclude_Min or else Code > Exclude_Max then
            Trace (J) := Code;
            J := J + 1;
         end if;

         Pop_Success := Pop_Frame (Frame'Access);
      end loop;

      Len := J - 1;
   end Call_Chain;

   procedure Call_Chain
     (Traceback   : in out System.Traceback_Entries.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
   begin
      Call_Chain
        (Traceback'Address, Max_Len, Len,
         Exclude_Min, Exclude_Max,

         --  Skip one extra frame to skip the other Call_Chain entry as well

         Skip_Frames => Skip_Frames + 1);
   end Call_Chain;

end System.Traceback;
