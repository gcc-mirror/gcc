------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                             T A R G P A R M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2007, Free Software Foundation, Inc.         --
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

--  This package obtains parameters from the target runtime version of System,
--  to indicate parameters relevant to the target environment.

--  Is it right for this to be modified GPL???

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

--  The parameters acquired by this routine from system.ads fall into four
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

--     4. Other characterisitics of package System. At the current time the
--        only item in this category is whether type Address is private.

with Rident; use Rident;
with Namet;  use Namet;
with Types;  use Types;

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

   --  If a pragma Detect_Blocking appears, then the flag Opt.Detect_Blocking
   --  is set to True.

   --  if a pragma Suppress_Exception_Locations appears, then the flag
   --  Opt.Exception_Locations_Suppressed is set to True.

   --  If a pragma Profile with a valid profile argument appears, then
   --  the appropriate restrictions and policy flags are set.

   --  The only other pragma allowed is a pragma Restrictions that specifies
   --  a restriction that will be imposed on all units in the partition. Note
   --  that in this context, only one restriction can be specified in a single
   --  pragma, and the pragma must appear on its own on a single source line.

   --  If package System contains exactly the line "type Address is private;"
   --  then the flag Opt.Address_Is_Private is set True, otherwise this flag
   --  is set False.

   Restrictions_On_Target : Restrictions_Info := No_Restrictions;
   --  Records restrictions specified by system.ads. Only the Set and Value
   --  members are modified. The Violated and Count fields are never modified.
   --  Note that entries can be set either by a pragma Restrictions or by
   --  a pragma Profile.

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

   --------------------------
   -- Executable Extension --
   --------------------------

   Executable_Extension_On_Target : Name_Id := No_Name;
   --  Executable extension on the target. This name is useful for setting
   --  the executable extension in a dynamic way, e.g. depending on the
   --  run time used, rather than using a configure-time macro as done by
   --  Get_Target_Executable_Suffix. If not set (No_Name), instead use
   --  System.OS_Lib.Get_Target_Executable_Suffix.

   -----------------------
   -- Target Parameters --
   -----------------------

   --  The following parameters correspond to the variables defined in the
   --  private part of System (without the terminating _On_Target). Note
   --  that it is required that all parameters defined here be specified
   --  in the target specific version of system.ads. Thus, to add a new
   --  parameter, add it to all system*.ads files. (There is a defaulting
   --  mechanism, but we don't normally take advantage of it, as explained
   --  below.)

   --  The default values here are used if no value is found in system.ads.
   --  This should normally happen if the special version of system.ads used
   --  by the compiler itself is in use or if the value is only relevant to
   --  a particular target (e.g. OpenVMS, AAMP). The default values are
   --  suitable for use in normal environments. This approach allows the
   --  possibility of new versions of the compiler (possibly with new system
   --  parameters added) being used to compile older versions of the compiler
   --  sources, as well as avoiding duplicating values in all system-*.ads
   --  files for flags that are used on a few platforms only.

   --  All these parameters should be regarded as read only by all clients
   --  of the package. The only way they get modified is by calling the
   --  Get_Target_Parameters routine which reads the values from a provided
   --  text buffer containing the source of the system package.

   ----------------------------
   -- Special Target Control --
   ----------------------------

   --  The great majority of GNAT ports are based on GCC. The switches in
   --  This section indicate the use of some non-standard target back end
   --  or other special targetting requirements.

   AAMP_On_Target : Boolean := False;
   --  Set to True if target is AAMP

   OpenVMS_On_Target : Boolean := False;
   --  Set to True if target is OpenVMS

   type Virtual_Machine_Kind is (No_VM, JVM_Target, CLI_Target);
   VM_Target : Virtual_Machine_Kind := No_VM;
   --  Kind of virtual machine targetted

   -------------------------------
   -- Backend Arithmetic Checks --
   -------------------------------

   --  Divide and overflow checks are either done in the front end or
   --  back end. The front end will generate checks when required unless
   --  the corresponding parameter here is set to indicate that the back
   --  end will generate the required checks (or that the checks are
   --  automatically performed by the hardware in an appropriate form).

   Backend_Divide_Checks_On_Target : Boolean := False;
   --  Set True if the back end generates divide checks, or if the hardware
   --  checks automatically. Set False if the front end must generate the
   --  required tests using explicit expanded code.

   Backend_Overflow_Checks_On_Target : Boolean := False;
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

   --      On most implementations, back-end zero-cost exceptions are used.
   --      Otherwise, Front-End Longjmp/Setjmp approach is used.
   --      Note that there is a requirement that all Ada units in a partition
   --      be compiled with the same exception model.

   --    Control of Available Methods and Defaults

   --      The following switches specify whether ZCX is available, and
   --      whether it is enabled by default.

   ZCX_By_Default_On_Target : Boolean := False;
   --  Indicates if zero cost exceptions are active by default. If this
   --  variable is False, then the only possible exception method is the
   --  front-end setjmp/longjmp approach, and this is the default. If
   --  this variable is True, then GCC ZCX is used.

   GCC_ZCX_Support_On_Target  : Boolean := False;
   --  Indicates that the target supports GCC Exceptions

   ------------------------------------
   -- Run-Time Library Configuration --
   ------------------------------------

   --  In configurable run-time mode, the system run-time may not support
   --  the full Ada language. The effect of setting this switch is to let
   --  the compiler know that it is not surprising (i.e. the system is not
   --  misconfigured) if run-time library units or entities within units are
   --  not present in the run-time.

   Configurable_Run_Time_On_Target : Boolean := False;
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

   Suppress_Standard_Library_On_Target : Boolean := False;
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
   --    The calls to __gnat_initialize and __gnat_finalize are omitted
   --
   --    All finalization and initialization (controlled types) is omitted
   --
   --    The routine __gnat_handler_installed is not imported

   Preallocated_Stacks_On_Target : Boolean := False;
   --  If this flag is True, then the expander preallocates all task stacks
   --  at compile time. If the flag is False, then task stacks are not pre-
   --  allocated, and task stack allocation is the responsibility of the
   --  run-time (which typically delegates the task to the underlying
   --  operating system environment).

   ---------------------
   -- Duration Format --
   ---------------------

   --  By default, type Duration is a 64-bit fixed-point type with a delta
   --  and small of 10**(-9) (i.e. it is a count in nanoseconds. This flag
   --  allows that standard format to be modified.

   Duration_32_Bits_On_Target : Boolean := False;
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

   Support_64_Bit_Divides_On_Target : Boolean := True;
   --  If True, the back end supports 64-bit divide operations. If False, then
   --  the source program may not contain 64-bit divide operations. This is
   --  specifically useful in the zero foot-print case, where the issue is
   --  whether there is a hardware divide instruction for 64-bits so that
   --  no run-time support is required. It should always be set True if the
   --  necessary run-time support is present.

   Support_Aggregates_On_Target : Boolean := True;
   --  In the general case, the use of aggregates may generate calls
   --  to run-time routines in the C library, including memset, memcpy,
   --  memmove, and bcopy. This flag is set to True if these routines
   --  are available. If any of these routines is not available, then
   --  this flag is False, and the use of aggregates is not permitted.

   Support_Composite_Assign_On_Target : Boolean := True;
   --  The assignment of composite objects other than small records and
   --  arrays whose size is 64-bits or less and is set by an explicit
   --  size clause may generate calls to memcpy, memmove, and bcopy.
   --  If versions of all these routines are available, then this flag
   --  is set to True. If any of these routines is not available, then
   --  the flag is set False, and composite assignments are not allowed.

   Support_Composite_Compare_On_Target : Boolean := True;
   --  If this flag is True, then the back end supports bit-wise comparison
   --  of composite objects for equality, either generating inline code or
   --  calling appropriate (and available) run-time routines. If this flag
   --  is False, then the back end does not provide this support, and the
   --  front end uses component by component comparison for composites.

   Support_Long_Shifts_On_Target : Boolean := True;
   --  If True, the back end supports 64-bit shift operations. If False, then
   --  the source program may not contain explicit 64-bit shifts. In addition,
   --  the code generated for packed arrays will avoid the use of long shifts.

   --------------------
   -- Indirect Calls --
   --------------------

   Always_Compatible_Rep_On_Target : Boolean := True;
   --  If True, the Can_Use_Internal_Rep flag (see Einfo) is set to False in
   --  all cases. This corresponds to the traditional code generation
   --  strategy. False allows the front end to choose a policy that partly or
   --  entirely eliminates dynamically generated trampolines.

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

   --   GCC Stack-limit Mechanism

   --      This approach uses the GCC stack limits mechanism.
   --      It relies on comparing the stack pointer with the
   --      values of a global symbol. If the check fails, a
   --      trap is explicitly generated. The advantage is
   --      that the mechanism requires no memory protection,
   --      but operating system and run-time support are
   --      needed to manage the per-task values of the symbol.
   --      This is the default method after probing where it
   --      is available.

   --   GNAT Stack-limit Checking

   --      This method relies on comparing the stack pointer
   --      with per-task stack limits. If the check fails, an
   --      exception is explicitly raised. The advantage is
   --      that the method requires no extra system dependent
   --      runtime support and can be used on systems without
   --      memory protection as well, but at the cost of more
   --      overhead for doing the check. This is the fallback
   --      method if the above two are not supported.

   Stack_Check_Probes_On_Target : Boolean := False;
   --  Indicates if the GCC probing mechanism is used

   Stack_Check_Limits_On_Target : Boolean := False;
   --  Indicates if the GCC stack-limit mechanism is used

   --  Both flags cannot be simultaneously set to True. If neither
   --  is, the target independent fallback method is used.

   Stack_Check_Default_On_Target : Boolean := False;
   --  Indicates if stack checking is on by default

   ----------------------------
   -- Command Line Arguments --
   ----------------------------

   --  For most ports of GNAT, command line arguments are supported. The
   --  following flag is set to False for targets that do not support
   --  command line arguments (VxWorks and AAMP). Note that support of
   --  command line arguments is not required on such targets (RM A.15(13)).

   Command_Line_Args_On_Target : Boolean := True;
   --  Set False if no command line arguments on target. Note that if this
   --  is False in with Configurable_Run_Time_On_Target set to True, then
   --  this causes suppression of generation of the argv/argc variables
   --  used to record command line arguments.

   --  Similarly, most ports support the use of an exit status, but AAMP
   --  is an exception (as allowed by RM A.15(18-20))

   Exit_Status_Supported_On_Target : Boolean := True;
   --  Set False if returning of an exit status is not supported on target.
   --  Note that if this False in with Configurable_Run_Time_On_Target
   --  set to True, then this causes suppression of the gnat_exit_status
   --  variable used to record the exit status.

   -----------------------
   -- Main Program Name --
   -----------------------

   --  When the binder generates the main program to be used to create the
   --  executable, the main program name is main by default (to match the
   --  usual Unix practice). If this parameter is set to True, then the
   --  name is instead by default taken from the actual Ada main program
   --  name (just the name of the child if the main program is a child unit).
   --  In either case, this value can be overridden using -M name.

   Use_Ada_Main_Program_Name_On_Target : Boolean := False;
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

   --  Note: the compiler itself does not use floating-point, so the
   --  settings of the defaults here are not really relevant.

   --  Note: in some cases, proper support of some of these floating point
   --  features may require a specific switch (e.g. -mieee on the Alpha)
   --  to be used to obtain full RM compliant support.

   Denorm_On_Target : Boolean := False;
   --  Set to False on targets that do not reliably support denormals

   Machine_Rounds_On_Target : Boolean := True;
   --  Set to False for targets where S'Machine_Rounds is False

   Machine_Overflows_On_Target : Boolean := False;
   --  Set to True for targets where S'Machine_Overflows is True

   Signed_Zeros_On_Target : Boolean := True;
   --  Set to False on targets that do not reliably support signed zeros

   -------------------------------------------
   -- Boolean-Valued Fixed-Point Attributes --
   -------------------------------------------

   Fractional_Fixed_Ops_On_Target : Boolean := False;
   --  Set to True for targets that support fixed-by-fixed multiplication
   --  and division for fixed-point types with a small value equal to
   --  2 ** (-(T'Object_Size - 1)) and whose values have an absolute
   --  value less than 1.0.

   -----------------
   -- Data Layout --
   -----------------

   --  Normally when using the GCC backend, Gigi and GCC perform much of the
   --  data layout using the standard layout capabilities of GCC. If the
   --  parameter Backend_Layout is set to False, then the front end must
   --  perform all data layout. For further details see the package Layout.

   Frontend_Layout_On_Target : Boolean := False;
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
