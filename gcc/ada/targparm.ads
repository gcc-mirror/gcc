------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                             T A R G P A R M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1999-2001 Free Software Foundation, Inc.          --
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

--  This package obtains parameters from the target runtime version of
--  System, to indicate parameters relevant to the target environment.

--  Conceptually, these parameters could be obtained using rtsfind, but
--  we do not do this for three reasons:

--    1. Compiling System for every compilation wastes time
--    2. This compilation impedes debugging by adding extra compile steps
--    3. There are recursion problems coming from compiling System itself
--        or any of its children.

--  For all these reasons, we read in the source of System, and then scan
--  it at the text level to extract the parameter values.

--  Note however, that later on, when the ali file is written, we make sure
--  that the System file is at least parsed, so that the checksum is properly
--  computed and set in the ali file. This partially negates points 1 and 2
--  above although just parsing is quick and does not impact debugging much.

package Targparm is

   --  The following parameters correspond to the variables defined in the
   --  private part of System (without the terminating _On_Target). Note
   --  that it is required that all parameters defined here be specified
   --  in the target specific version of system.ads (there are no defaults).

   --  All these parameters should be regarded as read only by all clients
   --  of the package. The only way they get modified is by calling the
   --  Get_Target_Parameters routine which reads the values from System.

   -------------------------------
   -- Backend Arithmetic Checks --
   -------------------------------

   --  Divide and overflow checks are either done in the front end or
   --  back end. The front end will generate checks when required unless
   --  the corresponding parameter here is set to indicate that the back
   --  end will generate the required checks (or that the checks are
   --  automatically performed by the hardware in an appropriate form).

   Backend_Divide_Checks_On_Target : Boolean;
   --  Set True if the back end generates divide checks, or if the hardware
   --  checks automatically. Set False if the front end must generate the
   --  required tests using explicit expanded code.

   Backend_Overflow_Checks_On_Target : Boolean;
   --  Set True if the back end generates arithmetic overflow checks, or if
   --  the hardware checks automatically. Set False if the front end must
   --  generate the required tests using explicit expanded code.

   -----------------------------------
   -- Control of Exception Handling --
   -----------------------------------

   --  GNAT provides two methods of implementing exceptions:

   --    Longjmp/Setjmp (-gnatL)

   --      This approach uses longjmp/setjmp to handle exceptions. It
   --      uses less storage, and can often propagate exceptions faster,
   --      at the expense of (sometimes considerable) overhead in setting
   --      up an exception handler. This approach is available on all
   --      targets, and is the default where it is the only approach.

   --    Zero Cost (-gnatZ)

   --      This approach uses separate exception tables. These use extra
   --      storage, and exception propagation can be quite slow, but there
   --      is no overhead in setting up an exception handler (it is to this
   --      latter operation that the phrase zero-cost refers). This approach
   --      is only available on some targets, and is the default where it is
   --      available.

   ZCX_By_Default_On_Target : Boolean;
   --  Indicates if zero cost exceptions are active by default.

   GCC_ZCX_Support_On_Target  : Boolean;
   --  Indicates that when ZCX is active the mechanism to be used is the
   --  standard GCC ZCX mechanism  (introduced in GCC 3.1)

   Front_End_ZCX_Support_On_Target : Boolean;
   --  Indicates that when ZCX is active (and GCC_ZCX_Support is not set)
   --  the mechanism to be used is the GNAT front end specific ZCX mechanism

   ---------------------------------------
   -- High_Integrity (No Run Time) Mode --
   ---------------------------------------

   --  In High_Integrity mode, there is no system run-time, and the flag
   --  Opt.No_Run_Time is set so that the language is appropriately
   --  restricted to forbid construct that would generate run-time calls.

   High_Integrity_Mode_On_Target : Boolean;
   --  Indicates that this build is for a high integrity mode version of
   --  GNAT, so that no run time is permitted.

   -------------------------------
   -- Control of Stack Checking --
   -------------------------------

   --  GNAT provides two methods of implementing exceptions:

   --    GCC Probing Mechanism

   --      This approach uses the standard GCC mechanism for
   --      stack checking. The method assumes that accessing
   --      storage immediately beyond the end of the stack
   --      will result in a trap that is converted to a storage
   --      error by the runtime system. This mechanism has
   --      minimal overhead, but requires complex hardware,
   --      operating system and run-time support. Probing is
   --      the default method where it is available. The stack
   --      size for the environment task depends on the operating
   --      system and cannot be set in a system-independent way.

   --   GNAT Stack-limit Checking

   --      This method relies on comparing the stack pointer
   --      with per-task stack limits. If the check fails, an
   --      exception is explicitly raised. The advantage is
   --      that the method requires no extra system dependent
   --      runtime support and can be used on systems without
   --      memory protection as well, but at the cost of more
   --      overhead for doing the check. This method is the
   --      default on systems that lack complete support for
   --      probing.

   Stack_Check_Probes_On_Target : Boolean;
   --  Indicates if stack check probes are used, as opposed to the standard
   --  target independent comparison method.

   Stack_Check_Default_On_Target : Boolean;
   --  Indicates if stack checking is on by default

   ----------------------------
   -- Command Line Arguments --
   ----------------------------

   --  For most ports of GNAT, command line arguments are supported. The
   --  following flag is set to False for targets that do not support
   --  command line arguments (notably VxWorks).

   Command_Line_Args_On_Target : Boolean;
   --  Set False if no command line arguments on target

   --  Note: this is prepared for future use, but not yet used, since we
   --  do not yet have a way of propagating Targparm params to the binder

   -----------------------
   -- Main Program Name --
   -----------------------

   --  When the binder generates the main program to be used to create the
   --  executable, the main program name is main by default (to match the
   --  usual Unix practice). If this parameter is set to True, then the
   --  name is instead by default taken from the actual Ada main program
   --  name (just the name of the child if the main program is a child unit).
   --  In either case, this value can be overridden using -M name.

   Use_Ada_Main_Program_Name_On_Target : Boolean;
   --  Set True to use the Ada main program name as the main name

   --  Note: this is prepared for future use, but not yet used, since we
   --  do not yet have a way of propagating Targparm params to the binder

   ----------------------------
   -- Support of Long Shifts --
   ----------------------------

   --  In GNORT mode, we cannot call library routines, and in particular
   --  we cannot call routines for long (64-bit) shifts if such routines
   --  are required on the target. This comes up in the context of support
   --  of packed arrays. We can only represent packed arrays whose length
   --  is in the range 33- to 64-bits as modular types if long shifts are
   --  done with inline code.

   --  For the default version, for now we set long shifts inlined as True
   --  This may not be quite accurate, but until we get proper separate
   --  System's for each target, it is a safer choice.

   Long_Shifts_Inlined_On_Target : Boolean;
   --  Indicates if long (double word) shifts are generated using inlined
   --  code (and thus are permissible in No_Run_Time mode).

   ----------------------------------------------
   -- Boolean-Valued Floating-Point Attributes --
   ----------------------------------------------

   --  The constants below give the values for representation oriented
   --  floating-point attributes that are the same for all float types
   --  on the target. These are all boolean values.

   --  A value is only True if the target reliably supports the corresponding
   --  feature. Reliably here means that support is guaranteed for all
   --  possible settings of the relevant compiler switches (like -mieee),
   --  since we cannot control the user setting of those switches.

   --  The attributes cannot dependent on the current setting of compiler
   --  switches, since the values must be static and consistent throughout
   --  the partition. We probably should add such consistency checks in future,
   --  but for now we don't do this.

   AAMP_On_Target : Boolean;
   --  Set to True if target is AAMP.

   Denorm_On_Target : Boolean;
   --  Set to False on targets that do not reliably support denormals.
   --  Reliably here means for all settings of the relevant -m flag, so
   --  for example, this is False on the Alpha where denormals are not
   --  supported unless -mieee is used.

   Machine_Rounds_On_Target : Boolean;
   --  Set to False for targets where S'Machine_Rounds is False

   Machine_Overflows_On_Target : Boolean;
   --  Set to True for targets where S'Machine_Overflows is True

   Signed_Zeros_On_Target : Boolean;
   --  Set to False on targets that do not reliably support signed zeros.

   OpenVMS_On_Target : Boolean;
   --  Set to True if target is OpenVMS.

   -------------------------------------------
   -- Boolean-Valued Fixed-Point Attributes --
   -------------------------------------------

   Fractional_Fixed_Ops_On_Target : Boolean;
   --  Set to True for targets that support fixed-by-fixed multiplication
   --  and division for fixed-point types with a small value equal to
   --  2 ** (-(T'Object_Size - 1)) and whose values have an absolute
   --  value less than 1.0.

   --------------------------------------------------------------
   -- Handling of Unconstrained Values Returned from Functions --
   --------------------------------------------------------------

   --  Functions that return variable length objects, notably unconstrained
   --  arrays are a special case, because there is no simple obvious way of
   --  implementing this feature. Furthermore, this capability is not present
   --  in C++ or C, so typically the system ABI does not handle this case.

   --  GNAT uses two different approaches

   --    The Secondary Stack

   --      The secondary stack is a special storage pool that is used for
   --      this purpose. The called function places the result on the
   --      secondary stack, and the caller uses or copies the value from
   --      the secondary stack, and pops the secondary stack after the
   --      value is consumed. The secondary stack is outside the system
   --      ABI, and the important point is that although generally it is
   --      handled in a stack like manner corresponding to the subprogram
   --      call structure, a return from a function does NOT pop the stack.

   --    DSP (Depressed Stack Pointer)

   --      Some targets permit the implementation of a function call/return
   --      protocol in which the function does not pop the main stack pointer
   --      on return, but rather returns with the stack pointer depressed.
   --      This is not generally permitted by any ABI, but for at least some
   --      targets, the implementation of alloca provides a model for this
   --      approach. If return-with-DSP is implemented, then functions that
   --      return variable length objects do it by returning with the stack
   --      pointer depressed, and the returned object is a pointer to the
   --      area within the stack frame of the called procedure that contains
   --      the returned value. The caller must then pop the main stack when
   --      this value is consumed.

   Functions_Return_By_DSP_On_Target : Boolean;
   --  Set to True if target permits functions to return with using the
   --  DSP (depressed stack pointer) approach.

   -----------------
   -- Data Layout --
   -----------------

   --  Normally when using the GCC backend, Gigi and GCC perform much of the
   --  data layout using the standard layout capabilities of GCC. If the
   --  parameter Backend_Layout is set to False, then the front end must
   --  perform all data layout. For further details see the package Layout.

   Frontend_Layout_On_Target : Boolean;
   --  Set True if front end does layout

   -----------------
   -- Subprograms --
   -----------------

   procedure Get_Target_Parameters;
   --  Called at the start of execution to read the source of System and
   --  obtain and set the values of the above parameters.

end Targparm;
