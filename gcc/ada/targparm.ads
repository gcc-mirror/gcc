------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                             T A R G P A R M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2003 Free Software Foundation, Inc.          --
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
--  we do not do this for four reasons:

--    1. Compiling System for every compilation wastes time

--    2. This compilation impedes debugging by adding extra compile steps

--    3. There are recursion problems coming from compiling System itself
--        or any of its children.

--    4. The binder also needs the parameters, and we do not want to have
--        to drag a lot of front end stuff into the binder.

--  For all these reasons, we read in the source of System, and then scan
--  it at the text level to extract the parameter values.

--  Note however, that later on, when the ali file is written, we make sure
--  that the System file is at least parsed, so that the checksum is properly
--  computed and set in the ali file. This partially negates points 1 and 2
--  above although just parsing is quick and does not impact debugging much.

--  The parameters acquired by this routine from system.ads fall into three
--  categories:

--     1. Configuration pragmas, that must appear at the start of the file.
--        Any such pragmas automatically apply to any unit compiled in the
--        presence of this system file. Only a limited set of such pragmas
--        may appear as documented in the corresponding section below,

--     2. Target parameters. These are boolean constants that are defined
--        in the private part of the package giving fixed information
--        about the target architecture, and the capabilities of the
--        code generator and run-time library.

--     3. Identification information. This is an optional string constant
--        that gives the name of the run-time library configuration. This
--        line may be ommitted for a version of system.ads to be used with
--        the full Ada 95 run time.

with Rident; use Rident;
with Types;  use Types;
with Uintp;  use Uintp;

package Targparm is

   ---------------------------
   -- Configuration Pragmas --
   ---------------------------

   --  The following switches get set if the corresponding configuration
   --  pragma is scanned from the source of system.ads. No other pragmas
   --  are permitted to appear at the start of the system.ads source file.

   --  If a pragma Discard_Names appears, then Opt.Global_Discard_Names is
   --  set to True to indicate that all units must be compiled in this mode.

   --  If a pragma Locking_Policy appears, then Opt.Locking_Policy is set
   --  to the first character of the policy name, and Opt.Locking_Policy_Sloc
   --  is set to System_Location.

   --  If a pragma Normalize_Scalars appears, then Opt.Normalize_Scalars
   --  is set True, as well as Opt.Init_Or_Norm_Scalars.

   --  If a pragma Queuing_Policy appears, then Opt.Queuing_Policy is set
   --  to the first character of the policy name, and Opt.Queuing_Policy_Sloc
   --  is set to System_Location.

   --  If a pragma Task_Dispatching_Policy appears, then the flag
   --  Opt.Task_Dispatching_Policy is set to the first character of the
   --  policy name, and Opt.Task_Dispatching_Policy_Sloc is set to
   --  System_Location.

   --  If a pragma Polling (On) appears, then the flag Opt.Polling_Required
   --  is set to True.

   --  if a pragma Suppress_Exception_Locations appears, then the flag
   --  Opt.Exception_Locations_Suppressed is set to True.

   --  The only other pragma allowed is a pragma Restrictions that gives the
   --  simple name of a restriction for which partition consistency is always
   --  required (see definition of Rident.Partition_Restrictions).

   Restrictions_On_Target :
     array (Partition_Restrictions) of Boolean := (others => False);
   --  Element is set True if a pragma Restrictions for the corresponding
   --  identifier appears in system.ads. Note that only partition restriction
   --  identifiers are permitted as arguments for pragma Restrictions for
   --  pragmas appearing at the start of system.ads.

   Restriction_Parameters_On_Target :
     array (Restriction_Parameter_Id) of Uint := (others => No_Uint);
   --  Element is set to specified value if a pragma Restrictions for the
   --  corresponding restriction parameter value is set.

   -------------------
   -- Run Time Name --
   -------------------

   --  This parameter should be regarded as read only by all clients of
   --  of package. The only way they get modified is by calling the
   --  Get_Target_Parameters routine which reads the values from a provided
   --  text buffer containing the source of the system package.

   --  The corresponding string constant is placed immediately at the start
   --  of the private part of system.ads if is present, e.g. in the form:

   --    Run_Time_Name : constant String := "Zero Footprint Run Time";

   --  the corresponding messages will look something like

   --    xxx not supported (Zero Footprint Run Time)

   Run_Time_Name_On_Target : Name_Id := No_Name;
   --  Set to appropriate names table entry Id value if a Run_Time_Name
   --  string constant is defined in system.ads. This name is used only
   --  for the configurable run-time case, and is used to parametrize
   --  messages that complain about non-supported run-time features.
   --  The name should contain only letters A-Z, digits 1-9, spaces,
   --  and underscores.

   -----------------------
   -- Target Parameters --
   -----------------------

   --  The following parameters correspond to the variables defined in the
   --  private part of System (without the terminating _On_Target). Note
   --  that it is required that all parameters defined here be specified
   --  in the target specific version of system.ads (there are no defaults).

   --  All these parameters should be regarded as read only by all clients
   --  of the package. The only way they get modified is by calling the
   --  Get_Target_Parameters routine which reads the values from a provided
   --  text buffer containing the source of the system package.

   ----------------------------
   -- Special Target Control --
   ----------------------------

   --  The great majority of GNAT ports are based on GCC. The switches in
   --  This section indicate the use of some non-standard target back end.

   AAMP_On_Target : Boolean;
   --  Set to True if target is AAMP.

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

   --  GNAT implements three methods of implementing exceptions:

   --    Front-End Longjmp/Setjmp Exceptions

   --      This approach uses longjmp/setjmp to handle exceptions. It
   --      uses less storage, and can often propagate exceptions faster,
   --      at the expense of (sometimes considerable) overhead in setting
   --      up an exception handler. This approach is available on all
   --      targets, and is the default where it is the only approach.

   --      The generation of the setjmp and longjmp calls is handled by
   --      the front end of the compiler (this includes gigi in the case
   --      of the standard GCC back end). It does not use any back end
   --      suport (such as the GCC3 exception handling mechanism). When
   --      this approach is used, the compiler generates special exception
   --      handlers for handling cleanups when an exception is raised.

   --    Front-End Zero Cost Exceptions

   --      This approach uses separate exception tables. These use extra
   --      storage, and exception propagation can be quite slow, but there
   --      is no overhead in setting up an exception handler (it is to this
   --      latter operation that the phrase zero-cost refers). This approach
   --      is only available on some targets, and is the default where it is
   --      available.

   --      The generation of the exception tables is handled by the front
   --      end of the compiler. It does not use any back end support (such
   --      as the GCC3 exception handling mechanism). When this approach
   --      is used, the compiler generates special exception handlers for
   --      handling cleanups when an exception is raised.

   --    Back-End Zero Cost Exceptions

   --      With this approach, the back end handles the generation and
   --      handling of exceptions. For example, the GCC3 exception handling
   --      mechanisms are used in this mode. The front end simply generates
   --      code for explicit exception handlers, and AT END cleanup handlers
   --      are simply passed unchanged to the backend for generating cleanups
   --      both in the exceptional and non-exceptional cases.

   --      As the name implies, this approach generally uses a zero-cost
   --      mechanism with tables, but the tables are generated by the back
   --      end. However, since the back-end is entirely responsible for the
   --      handling of exceptions, another mechanism might be used. In the
   --      case of GCC3 for instance, it might be the case that the compiler
   --      is configured for setjmp/longjmp handling, then everything will
   --      work correctly. However, it is definitely preferred that the
   --      back end provide zero cost exception handling.

   --    Controlling the selection of methods

   --      The Front-End Longjmp/Setjmp approach is always available in
   --      all implementations. If it is not the default method, then it
   --      may be explicitly specified by the use of -gnatL. Note however
   --      that there is a requirement that all Ada units in a partition
   --      be compiled with this overriding option if it is not the default.

   --      On some, but not all, implementations of GNAT, one of the two
   --      ZCX approaches (but not both) is implemented. If this is the
   --      case, and ZCX is not the default mechanism, then ZCX handling
   --      (front-end or back-end according to the implementation) may be
   --      specified by use of the -gnatZ switch. Again, this switch must
   --      be used to compile all Ada units in a partition. The use of
   --      the -gnatZ switch will cause termination with a fatal error.

   --      Finally the debug option -gnatdX can be used to force the
   --      compiler to operate in front-end ZCX exception mode and force
   --      the front end to generate exception tables. This is only useful
   --      for debugging purposes for implementations which do not provide
   --      the possibility of front-end ZCX mode. The resulting object file
   --      is unusable, but this debug switch may still be useful (e.g. in
   --      conjunction with -gnatG) for front-end debugging purposes.

   --    Control of Available Methods and Defaults

   --      The following switches specify which of the two ZCX methods
   --      (if any) is available in an implementation, and which method
   --      is the default method.

   ZCX_By_Default_On_Target : Boolean;
   --  Indicates if zero cost exceptions are active by default. If this
   --  variable is False, then the only possible exception method is the
   --  front-end setjmp/longjmp approach, and this is the default. If
   --  this variable is True, then one of the following two flags must
   --  be True, and represents the method to be used by default.

   GCC_ZCX_Support_On_Target  : Boolean;
   --  Indicates that when ZCX is active, the mechanism to be used is the
   --  back-end ZCX exception approach. If this variable is set to True,
   --  then Front_End_ZCX_Support_On_Target must be False.

   Front_End_ZCX_Support_On_Target : Boolean;
   --  Indicates that when ZCX is active, the mechanism to be used is the
   --  front-end ZCX exception approach. If this variable is set to True,
   --  then GCC_ZCX_Support_On_Target must be False.

   --------------------------------
   -- Configurable Run-Time Mode --
   --------------------------------

   --  In configurable run-time mode, the system run-time may not support
   --  the full Ada language. The effect of setting this switch is to let
   --  the compiler know that it is not surprising (i.e. the system is not
   --  misconfigured) if run-time library units or entities within units are
   --  not present in the run-time.

   Configurable_Run_Time_On_Target : Boolean;
   --  Indicates that the system.ads file is for a configurable run-time
   --
   --  This has some specific effects as follows
   --
   --    The binder generates the gnat_argc/argv/envp variables in the
   --    binder file instead of being imported from the run-time library.
   --    If Command_Line_Args_On_Target is set to False, then the
   --    generation of these variables is suppressed completely.
   --
   --    The binder generates the gnat_exit_status variable in the binder
   --    file instead of being imported from the run-time library. If
   --    Exit_Status_Supported_On_Target is set to False, then the
   --    generation of this variable is suppressed entirely.
   --
   --    The routine __gnat_break_start is defined within the binder file
   --    instead of being imported from the run-time library.
   --
   --    The variable __gnat_exit_status is generated within the binder file
   --    instead of being imported from the run-time library.

   Suppress_Standard_Library_On_Target : Boolean;
   --  If this flag is True, then the standard library is not included by
   --  default in the executable (see unit System.Standard_Library in file
   --  s-stalib.ads for details of what this includes). This is for example
   --  set True for the zero foot print case, where these files should not
   --  be included by default.
   --
   --  This flag has some other related effects:
   --
   --    The generation of global variables in the bind file is suppressed,
   --    with the exception of the priority of the environment task, which
   --    is needed by the Ravenscar run-time.
   --
   --    The generation of exception tables is suppressed for front end
   --    ZCX exception handling (since we assume no exception handling).
   --
   --    The calls to __gnat_initialize and __gnat_finalize are omitted
   --
   --    All finalization and initialization (controlled types) is omitted
   --
   --    The routine __gnat_handler_installed is not imported

   ---------------------
   -- Duration Format --
   ---------------------

   --  By default, type Duration is a 64-bit fixed-point type with a delta
   --  and small of 10**(-9) (i.e. it is a count in nanoseconds. This flag
   --  allows that standard format to be modified.

   Duration_32_Bits_On_Target : Boolean;
   --  If True, then Duration is represented in 32 bits and the delta and
   --  small values are set to 20.0*(10**(-3)) (i.e. it is a count in units
   --  of 20 milliseconds.

   ------------------------------------
   -- Back-End Code Generation Flags --
   ------------------------------------

   --  These flags indicate possible limitations in what the code generator
   --  can handle. They will all be True for a full run-time, but one or more
   --  of these may be false for a configurable run-time, and if a feature is
   --  used at the source level, and the corresponding flag is false, then an
   --  error message will be issued saying the feature is not supported.

   Support_64_Bit_Divides_On_Target : Boolean;
   --  If True, the back end supports 64-bit divide operations. If False, then
   --  the source program may not contain 64-bit divide operations. This is
   --  specifically useful in the zero foot-print case, where the issue is
   --  whether there is a hardware divide instruction for 64-bits so that
   --  no run-time support is required. It should always be set True if the
   --  necessary run-time support is present.

   Support_Aggregates_On_Target : Boolean;
   --  In the general case, the use of aggregates may generate calls
   --  to run-time routines in the C library, including memset, memcpy,
   --  memmove, and bcopy. This flag is set to True if these routines
   --  are available. If any of these routines is not available, then
   --  this flag is False, and the use of aggregates is not permitted.

   Support_Composite_Assign_On_Target : Boolean;
   --  The assignment of composite objects other than small records and
   --  arrays whose size is 64-bits or less and is set by an explicit
   --  size clause may generate calls to memcpy, memmove, and bcopy.
   --  If versions of all these routines are available, then this flag
   --  is set to True. If any of these routines is not available, then
   --  the flag is set False, and composite assignments are not allowed.

   Support_Composite_Compare_On_Target : Boolean;
   --  If this flag is True, then the back end supports bit-wise comparison
   --  of composite objects for equality, either generating inline code or
   --  calling appropriate (and available) run-time routines. If this flag
   --  is False, then the back end does not provide this support, and the
   --  front end uses component by component comparison for composites.

   Support_Long_Shifts_On_Target : Boolean;
   --  If True, the back end supports 64-bit shift operations. If False, then
   --  the source program may not contain explicit 64-bit shifts. In addition,
   --  the code generated for packed arrays will avoid the use of long shifts.

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
   --  command line arguments (VxWorks and AAMP). Note that support of
   --  command line arguments is not required on such targets (RM A.15(13)).

   Command_Line_Args_On_Target : Boolean;
   --  Set False if no command line arguments on target. Note that if this
   --  is False in with Configurable_Run_Time_On_Target set to True, then
   --  this causes suppression of generation of the argv/argc variables
   --  used to record command line arguments.

   --  Similarly, most ports support the use of an exit status, but AAMP
   --  is an exception (as allowed by RM A.15(18-20))

   Exit_Status_Supported_On_Target : Boolean;
   --  Set False if returning of an exit status is not supported on target.
   --  Note that if this False in with Configurable_Run_Time_On_Target
   --  set to True, then this causes suppression of the gnat_exit_status
   --  variable used to recod the exit status.

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

   --  These subprograms are used to initialize the target parameter values
   --  from the system.ads file. Note that this is only done once, so if more
   --  than one call is made to either routine, the second and subsequent
   --  calls are ignored.

   procedure Get_Target_Parameters
     (System_Text  : Source_Buffer_Ptr;
      Source_First : Source_Ptr;
      Source_Last  : Source_Ptr);
   --  Called at the start of execution to obtain target parameters from
   --  the source of package System. The parameters provide the source
   --  text to be scanned (in System_Text (Source_First .. Source_Last)).

   procedure Get_Target_Parameters;
   --  This version reads in system.ads using Osint. The idea is that the
   --  caller uses the first version if they have to read system.ads anyway
   --  (e.g. the compiler) and uses this simpler interface if system.ads is
   --  not otherwise needed.

end Targparm;
