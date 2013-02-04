------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . T A S K I N G . D E B U G                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 2008-2012, Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  OpenVMS Version

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Aux_DEC;
with System.CRTL;
with System.Task_Primitives.Operations;
package body System.Tasking.Debug is

   package OSI renames System.OS_Interface;
   package STPO renames System.Task_Primitives.Operations;

   use System.Aux_DEC;

   --  Condition value type

   subtype Cond_Value_Type is Unsigned_Longword;

   type Trace_Flag_Set is array (Character) of Boolean;

   Trace_On : Trace_Flag_Set := ('A' .. 'Z' => False, others => True);

   --  Print_Routine fuction codes

   type Print_Functions is
     (No_Print, Print_Newline, Print_Control,
      Print_String, Print_Symbol, Print_FAO);
   for Print_Functions use
     (No_Print => 0, Print_Newline => 1, Print_Control => 2,
      Print_String => 3, Print_Symbol => 4, Print_FAO => 5);

   --  Counted ascii type declarations

   subtype Count_Type is Natural range 0 .. 255;
   for Count_Type'Object_Size use 8;

   type ASCIC (Count : Count_Type) is record
      Text  : String (1 .. Count);
   end record;

   for ASCIC use record
      Count at 0 range 0 .. 7;
   end record;
   pragma Pack (ASCIC);

   type AASCIC is access ASCIC;
   for AASCIC'Size use 32;

   type AASCIC_Array is array (Positive range <>) of AASCIC;

   type ASCIC127 is record
      Count : Count_Type;
      Text  : String (1 .. 127);
   end record;

   for ASCIC127 use record
      Count at 0 range 0 .. 7;
      Text  at 1 range 0 .. 127 * 8 - 1;
   end record;

   --  DEBUG Event record types used to signal DEBUG about Ada events

   type Debug_Event_Record is record
      Code     : Unsigned_Word; --  Event code that uniquely identifies event
      Flags    : Bit_Array_8;   --  Flag bits
      --                            Bit 0: This event allows a parameter list
      --                            Bit 1: Parameters are address expressions
      Sentinal : Unsigned_Byte; --  Sentinal valuye: Always K_EVENT_SENT
      TS_Kind  : Unsigned_Byte; --  DST type specification: Always K_TS_TASK
      DType    : Unsigned_Byte; --  DTYPE of parameter if of atomic data type
      --                            Always K_DTYPE_TASK
      MBZ      : Unsigned_Byte; --  Unused (must be zero)
      Minchr   : Count_Type;    --  Minimum chars needed to identify event
      Name     : ASCIC (31);    --  Event name uppercase only
      Help     : AASCIC;        --  Event description
   end record;

   for Debug_Event_Record use record
      Code     at 0 range 0 .. 15;
      Flags    at 2 range 0 .. 7;
      Sentinal at 3 range 0 .. 7;
      TS_Kind  at 4 range 0 .. 7;
      Dtype    at 5 range 0 .. 7;
      MBZ      at 6 range 0 .. 7;
      Minchr   at 7 range 0 .. 7;
      Name     at 8 range 0 .. 32 * 8 - 1;
      Help     at 40 range 0 .. 31;
   end record;

   type Ada_Event_Control_Block_Type is record
      Code      : Unsigned_Word;     --  Reserved and defined by DEBUG
      Unused1   : Unsigned_Byte;     --  Reserved and defined by DEBUG
      Sentinal  : Unsigned_Byte;     --  Reserved and defined by DEBUG
      Facility  : Unsigned_Word;     --  Reserved and defined by DEBUG
      Flags     : Unsigned_Word;     --  Reserved and defined by DEBUG
      Value     : Unsigned_Longword; --  Reserved and defined by DEBUG
      Unused2   : Unsigned_Longword; --  Reserved and defined by DEBUG
      Sigargs   : Unsigned_Longword;
      P1        : Unsigned_Longword;
      Sub_Event : Unsigned_Longword;
   end record;

   for Ada_Event_Control_Block_Type use record
      Code      at 0 range 0 .. 15;
      Unused1   at 2 range 0 .. 7;
      Sentinal  at 3 range 0 .. 7;
      Facility  at 4 range 0 .. 15;
      Flags     at 6 range 0 .. 15;
      Value     at 8 range 0 .. 31;
      Unused2   at 12 range 0 .. 31;
      Sigargs   at 16 range 0 .. 31;
      P1        at 20 range 0 .. 31;
      Sub_Event at 24 range 0 .. 31;
   end record;

   type Ada_Event_Control_Block_Access is access Ada_Event_Control_Block_Type;
   for Ada_Event_Control_Block_Access'Size use 32;

   --  Print_Routine_Type with max optional parameters

   type Print_Routine_Type is access procedure
     (Print_Function    : Print_Functions;
      Print_Subfunction : Print_Functions;
      P1                : Unsigned_Longword := 0;
      P2                : Unsigned_Longword := 0;
      P3                : Unsigned_Longword := 0;
      P4                : Unsigned_Longword := 0;
      P5                : Unsigned_Longword := 0;
      P6                : Unsigned_Longword := 0);
   for Print_Routine_Type'Size use 32;

   ---------------
   -- Constants --
   ---------------

   --  These are used to obtain and convert task values
   K_CVT_VALUE_NUM  : constant := 1;
   K_CVT_NUM_VALUE  : constant := 2;
   K_NEXT_TASK      : constant := 3;

   --  These are used to ask ADA to display task information
   K_SHOW_TASK     : constant := 4;
   K_SHOW_STAT     : constant := 5;
   K_SHOW_DEADLOCK : constant := 6;

   --  These are used to get and set various attributes of one or more tasks
   --    Task state
   --  K_GET_STATE  : constant := 7;
   --  K_GET_ACTIVE : constant := 8;
   --  K_SET_ACTIVE : constant := 9;
   K_SET_ABORT  : constant := 10;
   --  K_SET_HOLD   : constant := 11;

   --    Task priority
   K_GET_PRIORITY      : constant := 12;
   K_SET_PRIORITY      : constant := 13;
   K_RESTORE_PRIORITY  : constant := 14;

   --    Task registers
   --  K_GET_REGISTERS     : constant := 15;
   --  K_SET_REGISTERS     : constant := 16;

   --  These are used to control definable events
   K_ENABLE_EVENT   : constant := 17;
   K_DISABLE_EVENT  : constant := 18;
   K_ANNOUNCE_EVENT : constant := 19;

   --  These are used to control time-slicing.
   --  K_SHOW_TIME_SLICE : constant := 20;
   --  K_SET_TIME_SLICE  : constant := 21;

   --  This is used to symbolize task stack addresses.
   --  K_SYMBOLIZE_ADDRESS : constant := 22;

   K_GET_CALLER : constant := 23;
   --  This is used to obtain the task value of the caller task

   --  Miscellaneous functions - see below for details

   K_CLEANUP_EVENT  : constant := 24;
   K_SHOW_EVENT_DEF : constant := 25;
   --  K_CHECK_TASK_STACK : constant := 26;  --  why commented out ???

   --  This is used to obtain the DBGEXT-interface revision level
   --  K_GET_DBGEXT_REV : constant := 27; -- why commented out ???

   K_GET_STATE_1 : constant := 28;
   --  This is used to obtain additional state info, primarily for PCA

   K_FIND_EVENT_BY_CODE : constant := 29;
   K_FIND_EVENT_BY_NAME : constant := 30;
   --  These are used to search for user-defined event entries

   --  This is used to stop task schedulding. Why commented out ???
   --  K_STOP_ALL_OTHER_TASKS : constant := 31;

   --  Debug event constants

   K_TASK_NOT_EXIST  : constant := 3;
   K_SUCCESS         : constant := 1;
   K_EVENT_SENT      : constant := 16#9A#;
   K_TS_TASK         : constant := 18;
   K_DTYPE_TASK      : constant := 44;

   --  Status signal constants

   SS_BADPARAM       : constant := 20;
   SS_NORMAL         : constant := 1;

   --  Miscellaneous mask constants

   V_EVNT_ALL        : constant := 0;
   V_Full_Display    : constant := 11;
   V_Suppress_Header : constant := 13;

   --  CMA constants (why are some commented out???)

   CMA_C_DEBGET_GUARDSIZE     : constant := 1;
   CMA_C_DEBGET_IS_HELD       : constant := 2;
--   CMA_C_DEBGET_IS_INITIAL    : constant := 3;
--   CMA_C_DEBGET_NUMBER        : constant := 4;
   CMA_C_DEBGET_STACKPTR      : constant := 5;
   CMA_C_DEBGET_STACK_BASE    : constant := 6;
   CMA_C_DEBGET_STACK_TOP     : constant := 7;
   CMA_C_DEBGET_SCHED_STATE   : constant := 8;
   CMA_C_DEBGET_YELLOWSIZE    : constant := 9;
--   CMA_C_DEBGET_BASE_PRIO     : constant := 10;
--   CMA_C_DEBGET_REGS          : constant := 11;
--   CMA_C_DEBGET_ALT_PENDING   : constant := 12;
--   CMA_C_DEBGET_ALT_A_ENABLE  : constant := 13;
--   CMA_C_DEBGET_ALT_G_ENABLE  : constant := 14;
--   CMA_C_DEBGET_SUBSTATE      : constant := 15;
--   CMA_C_DEBGET_OBJECT_ADDR   : constant := 16;
--   CMA_C_DEBGET_THKIND        : constant := 17;
--   CMA_C_DEBGET_DETACHED      : constant := 18;
   CMA_C_DEBGET_TCB_SIZE      : constant := 19;
--   CMA_C_DEBGET_START_PC      : constant := 20;
--   CMA_C_DEBGET_NEXT_PC       : constant := 22;
--   CMA_C_DEBGET_POLICY        : constant := 23;
--   CMA_C_DEBGET_STACK_YELLOW  : constant := 24;
--   CMA_C_DEBGET_STACK_DEFAULT : constant := 25;

   --  Miscellaneous counted ascii constants

   Star     : constant AASCIC := new ASCIC'(2, ("* "));
   NoStar   : constant AASCIC := new ASCIC'(2, ("  "));
   Hold     : constant AASCIC := new ASCIC'(4, ("HOLD"));
   NoHold   : constant AASCIC := new ASCIC'(4, ("    "));
   Header   : constant AASCIC := new ASCIC '
     (60, ("  task id     pri hold state   substate          task object"));
   Empty_Text : constant AASCIC := new ASCIC (0);

   --  DEBUG Ada tasking states equated to their GNAT tasking equivalents

   Ada_State_Invalid_State     : constant AASCIC :=
     new ASCIC'(17, "Invalid state    ");
--   Ada_State_Abnormal          : constant AASCIC :=
--     new ASCIC'(17, "Abnormal         ");
   Ada_State_Aborting          : constant AASCIC :=
     new ASCIC'(17, "Aborting         "); --  Aborting (new)
--   Ada_State_Completed_Abn     : constant AASCIC :=
--     new ASCIC'(17, "Completed  [abn] ");
--   Ada_State_Completed_Exc     : constant AASCIC :=
--     new ASCIC'(17, "Completed  [exc] ");
   Ada_State_Completed         : constant AASCIC :=
     new ASCIC'(17, "Completed        "); --  Master_Completion_Sleep
   Ada_State_Runnable          : constant AASCIC :=
     new ASCIC'(17, "Runnable         "); --  Runnable
   Ada_State_Activating        : constant AASCIC :=
     new ASCIC'(17, "Activating       ");
   Ada_State_Accept            : constant AASCIC :=
     new ASCIC'(17, "Accept           "); --  Acceptor_Sleep
   Ada_State_Select_or_Delay   : constant AASCIC :=
     new ASCIC'(17, "Select or delay  "); --  Acceptor_Delay_Sleep
   Ada_State_Select_or_Term    : constant AASCIC :=
     new ASCIC'(17, "Select or term.  "); -- Terminate_Alternative
   Ada_State_Select_or_Abort   : constant AASCIC :=
     new ASCIC'(17, "Select or abort  "); --  Async_Select_Sleep (new)
--   Ada_State_Select            : constant AASCIC :=
--     new ASCIC'(17, "Select           ");
   Ada_State_Activating_Tasks  : constant AASCIC :=
     new ASCIC'(17, "Activating tasks "); --  Activator_Sleep
   Ada_State_Delay             : constant AASCIC :=
     new ASCIC'(17, "Delay            "); --  AST_Pending
--   Ada_State_Dependents        : constant AASCIC :=
--     new ASCIC'(17, "Dependents       ");
   Ada_State_Entry_Call        : constant AASCIC :=
     new ASCIC'(17, "Entry call       "); --  Entry_Caller_Sleep
   Ada_State_Cond_Entry_Call   : constant AASCIC :=
     new ASCIC'(17, "Cond. entry call "); --  Call.Mode.Conditional_Call
   Ada_State_Timed_Entry_Call  : constant AASCIC :=
     new ASCIC'(17, "Timed entry call "); --  Call.Mode.Timed_Call
   Ada_State_Async_Entry_Call  : constant AASCIC :=
     new ASCIC'(17, "Async entry call "); --  Call.Mode.Asynchronous_Call (new)
--   Ada_State_Dependents_Exc    : constant AASCIC :=
--     new ASCIC'(17, "Dependents [exc] ");
   Ada_State_IO_or_AST         : constant AASCIC :=
     new ASCIC'(17, "I/O or AST       "); --  AST_Server_Sleep
--   Ada_State_Shared_Resource   : constant AASCIC :=
--     new ASCIC'(17, "Shared resource  ");
   Ada_State_Not_Yet_Activated : constant AASCIC :=
     new ASCIC'(17, "Not yet activated"); --  Unactivated
--   Ada_State_Terminated_Abn    : constant AASCIC :=
--     new ASCIC'(17, "Terminated [abn] ");
--   Ada_State_Terminated_Exc    : constant AASCIC :=
--     new ASCIC'(17, "Terminated [exc] ");
   Ada_State_Terminated        : constant AASCIC :=
     new ASCIC'(17, "Terminated       "); --  Terminated
   Ada_State_Server            : constant AASCIC :=
     new ASCIC'(17, "Server           "); --  Servers
   Ada_State_Async_Hold        : constant AASCIC :=
     new ASCIC'(17, "Async_Hold       "); --  Async_Hold

   --  Task state counted ascii constants

   Debug_State_Emp : constant AASCIC := new ASCIC'(5, "     ");
   Debug_State_Run : constant AASCIC := new ASCIC'(5, "RUN  ");
   Debug_State_Rea : constant AASCIC := new ASCIC'(5, "READY");
   Debug_State_Sus : constant AASCIC := new ASCIC'(5, "SUSP ");
   Debug_State_Ter : constant AASCIC := new ASCIC'(5, "TERM ");

   --  Priority order of event display

   Global_Event_Display_Order : constant array (Event_Kind_Type)
     of Event_Kind_Type := (
      Debug_Event_Abort_Terminated,
      Debug_Event_Activating,
      Debug_Event_Dependents_Exception,
      Debug_Event_Exception_Terminated,
      Debug_Event_Handled,
      Debug_Event_Handled_Others,
      Debug_Event_Preempted,
      Debug_Event_Rendezvous_Exception,
      Debug_Event_Run,
      Debug_Event_Suspended,
      Debug_Event_Terminated);

   --  Constant array defining all debug events

   Event_Directory : constant array (Event_Kind_Type)
     of Debug_Event_Record := (
      (Debug_Event_Activating,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       2,
       (31, "ACTIVATING                     "),
       new ASCIC'(41, "!_a task is about to begin its activation")),

      (Debug_Event_Run,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       2,
       (31, "RUN                            "),
       new ASCIC'(24, "!_a task is about to run")),

      (Debug_Event_Suspended,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       1,
       (31, "SUSPENDED                      "),
       new ASCIC'(33, "!_a task is about to be suspended")),

      (Debug_Event_Preempted,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       1,
       (31, "PREEMPTED                      "),
       new ASCIC'(33, "!_a task is about to be preempted")),

      (Debug_Event_Terminated,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       1,
       (31, "TERMINATED                     "),
       new ASCIC'(57,
        "!_a task is terminating (including by abort or exception)")),

      (Debug_Event_Abort_Terminated,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       2,
       (31, "ABORT_TERMINATED               "),
       new ASCIC'(40, "!_a task is terminating because of abort")),

      (Debug_Event_Exception_Terminated,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       1,
       (31, "EXCEPTION_TERMINATED           "),
       new ASCIC'(47, "!_a task is terminating because of an exception")),

      (Debug_Event_Rendezvous_Exception,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       3,
       (31, "RENDEZVOUS_EXCEPTION           "),
       new ASCIC'(49, "!_an exception is propagating out of a rendezvous")),

      (Debug_Event_Handled,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       1,
       (31, "HANDLED                        "),
       new ASCIC'(37, "!_an exception is about to be handled")),

      (Debug_Event_Dependents_Exception,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       1,
       (31, "DEPENDENTS_EXCEPTION           "),
       new ASCIC'(64,
        "!_an exception is about to cause a task to await dependent tasks")),

      (Debug_Event_Handled_Others,
       (False, False, False, False, False, False, False, True),
       K_EVENT_SENT,
       K_TS_TASK,
       K_DTYPE_TASK,
       0,
       1,
       (31, "HANDLED_OTHERS                 "),
       new ASCIC'(58,
        "!_an exception is about to be handled in an OTHERS handler")));

   --  Help on events displayed in DEBUG

   Event_Def_Help : constant AASCIC_Array := (
     new ASCIC'(0,  ""),
     new ASCIC'(65,
      "  The general forms of commands to set a breakpoint or tracepoint"),
     new ASCIC'(22, "  on an Ada event are:"),
     new ASCIC'(73, "    SET BREAK/EVENT=event [task[, ... ]] " &
                    "[WHEN(expr)] [DO(comnd[; ... ])]"),
     new ASCIC'(73, "    SET TRACE/EVENT=event [task[, ... ]] " &
                    "[WHEN(expr)] [DO(comnd[; ... ])]"),
     new ASCIC'(0,  ""),
     new ASCIC'(65,
      "  If tasks are specified, the breakpoint will trigger only if the"),
     new ASCIC'(40, "  event occurs for those specific tasks."),
     new ASCIC'(0,  ""),
     new ASCIC'(39, "  Ada event names and their definitions"),
     new ASCIC'(0,  ""));

   -----------------------
   -- Package Variables --
   -----------------------

   AC_Buffer : ASCIC127;

   Events_Enabled_Count : Integer := 0;

   Print_Routine_Bufsiz : constant := 132;
   Print_Routine_Bufcnt : Integer := 0;
   Print_Routine_Linbuf : String (1 .. Print_Routine_Bufsiz);

   Global_Task_Debug_Events : Debug_Event_Array :=
     (False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False);
   --  Global table of task debug events set by the debugger

   --------------------------
   -- Exported Subprograms --
   --------------------------

   procedure Default_Print_Routine
     (Print_Function    : Print_Functions;
      Print_Subfunction : Print_Functions;
      P1                : Unsigned_Longword := 0;
      P2                : Unsigned_Longword := 0;
      P3                : Unsigned_Longword := 0;
      P4                : Unsigned_Longword := 0;
      P5                : Unsigned_Longword := 0;
      P6                : Unsigned_Longword := 0);
   --  The default print routine if not overridden.
   --  Print_Function determines option argument formatting.
   --  Print_Subfunction buffers output if No_Print, calls Put_Output if
   --  Print_Newline

   pragma Export_Procedure
     (Default_Print_Routine,
      Mechanism => (Value, Value, Reference, Reference, Reference));

   --------------------------
   -- Imported Subprograms --
   --------------------------

   procedure Debug_Get
     (Thread_Id : OSI.Thread_Id;
      Item_Req  : Unsigned_Word;
      Out_Buff  : System.Address;
      Buff_Siz  : Unsigned_Word);

   procedure Debug_Get
     (Thread_Id : OSI.Thread_Id;
      Item_Req  : Unsigned_Word;
      Out_Buff  : Unsigned_Longword;
      Buff_Siz  : Unsigned_Word);
   pragma Import (External, Debug_Get);

   pragma Import_Procedure (Debug_Get, "CMA$DEBUG_GET",
     (OSI.Thread_Id, Unsigned_Word, System.Address, Unsigned_Word),
     (Reference, Value, Reference, Value));

   pragma Import_Procedure (Debug_Get, "CMA$DEBUG_GET",
     (OSI.Thread_Id, Unsigned_Word, Unsigned_Longword, Unsigned_Word),
     (Reference, Value, Reference, Value));

   procedure FAOL
     (Status : out Cond_Value_Type;
      Ctrstr : String;
      Outlen : out Unsigned_Word;
      Outbuf : out String;
      Prmlst : Unsigned_Longword_Array);
   pragma Import (External, FAOL);

   pragma Import_Valued_Procedure (FAOL, "SYS$FAOL",
     (Cond_Value_Type, String, Unsigned_Word, String, Unsigned_Longword_Array),
     (Value, Descriptor (S), Reference, Descriptor (S), Reference));

   procedure Put_Output (
     Status         : out Cond_Value_Type;
     Message_String : String);

   procedure Put_Output (Message_String : String);
   pragma Import (External, Put_Output);

   pragma Import_Valued_Procedure (Put_Output, "LIB$PUT_OUTPUT",
     (Cond_Value_Type, String),
     (Value, Short_Descriptor (S)));

   pragma Import_Procedure (Put_Output, "LIB$PUT_OUTPUT",
     (String),
     (Short_Descriptor (S)));

   procedure Signal
     (Condition_Value     : Cond_Value_Type;
      Number_Of_Arguments : Integer := Integer'Null_Parameter;
      FAO_Argument_1      : Unsigned_Longword :=
                              Unsigned_Longword'Null_Parameter);
   pragma Import (External, Signal);

   pragma Import_Procedure (Signal, "LIB$SIGNAL",
      (Cond_Value_Type, Integer, Unsigned_Longword),
      (Value, Value, Value),
       Number_Of_Arguments);

   ----------------------------
   -- Generic Instantiations --
   ----------------------------

   function Fetch is new Fetch_From_Address (Unsigned_Longword);
   pragma Unreferenced (Fetch);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Ada_Event_Control_Block_Type,
      Name   => Ada_Event_Control_Block_Access);

   function To_AASCIC is new
     Ada.Unchecked_Conversion (Unsigned_Longword, AASCIC);

   function To_Addr is new
     Ada.Unchecked_Conversion (Task_Procedure_Access, Address);
   pragma Unreferenced (To_Addr);

   function To_EVCB is new
     Ada.Unchecked_Conversion
      (Unsigned_Longword, Ada_Event_Control_Block_Access);

   function To_Integer is new
     Ada.Unchecked_Conversion (Task_Id, System.Task_Primitives.Task_Address);

   function To_Print_Routine_Type is new
     Ada.Unchecked_Conversion (Short_Address, Print_Routine_Type);

   --  Optional argumements passed to Print_Routine have to be
   --  Unsigned_Longwords so define the required Unchecked_Conversions

   function To_UL is new
     Ada.Unchecked_Conversion (AASCIC, Unsigned_Longword);

   function To_UL is new
     Ada.Unchecked_Conversion (Integer, Unsigned_Longword);

   function To_UL is new
     Ada.Unchecked_Conversion (Task_Id, Unsigned_Longword);

   pragma Warnings (Off); --  Different sizes
   function To_UL is new
     Ada.Unchecked_Conversion (Task_Entry_Index, Unsigned_Longword);
   pragma Warnings (On);

   function To_UL is new
     Ada.Unchecked_Conversion (Short_Address, Unsigned_Longword);

   function To_UL is new
     Ada.Unchecked_Conversion
      (Ada_Event_Control_Block_Access, Unsigned_Longword);

   -----------------------
   -- Local Subprograms --
   -----------------------

   subtype Function_Codes is System.Aux_DEC.Unsigned_Word range 1 .. 31;
   --  The 31 function codes sent by the debugger needed to implement
   --  tasking support, enumerated below.

   type Register_Array is array (Natural range 0 .. 16) of
     System.Aux_DEC.Unsigned_Longword;
   --  The register array is a holdover from VAX and not used
   --  on Alpha or I64 but is kept as a filler below.

   type DBGEXT_Control_Block (Function_Code : Function_Codes) is record
      Facility_ID         : System.Aux_DEC.Unsigned_Word;
      --  For GNAT use the "Ada" facility ID
      Status              : System.Aux_DEC.Unsigned_Longword;
      --  Successful or otherwise returned status
      Flags               : System.Aux_DEC.Bit_Array_32;
      --   Used to flag event as global
      Print_Routine       : System.Aux_DEC.Short_Address;
      --  The print subprogram the caller wants to use for output
      Event_Code_or_EVCB  : System.Aux_DEC.Unsigned_Longword;
      --  Dual use Event Code or EVent Control Block
      Event_Value_or_Name : System.Aux_DEC.Unsigned_Longword;
      --  Dual use Event Value or Event Name string pointer
      Event_Entry         : System.Aux_DEC.Unsigned_Longword;
      Task_Value          : Task_Id;
      Task_Number         : Integer;
      Ada_Flags           : System.Aux_DEC.Bit_Array_32;
      Priority            : System.Aux_DEC.Bit_Array_32;
      Active_Registers    : System.Aux_DEC.Short_Address;

      case Function_Code is
         when K_GET_STATE_1 =>
            Base_Priority       : System.Aux_DEC.Bit_Array_32;
            Task_Type_Name      : System.Aux_DEC.Short_Address;
            Creation_PC         : System.Aux_DEC.Short_Address;
            Parent_Task_ID      : Task_Id;

         when others =>
            Ignored_Unused      : Register_Array;

      end case;
   end record;

   for DBGEXT_Control_Block use record
      Function_Code       at 0  range 0 .. 15;
      Facility_ID         at 2  range 0 .. 15;
      Status              at 4  range 0 .. 31;
      Flags               at 8  range 0 .. 31;
      Print_Routine       at 12 range 0 .. 31;
      Event_Code_or_EVCB  at 16 range 0 .. 31;
      Event_Value_or_Name at 20 range 0 .. 31;
      Event_Entry         at 24 range 0 .. 31;
      Task_Value          at 28 range 0 .. 31;
      Task_Number         at 32 range 0 .. 31;
      Ada_Flags           at 36 range 0 .. 31;
      Priority            at 40 range 0 .. 31;
      Active_Registers    at 44 range 0 .. 31;
      Ignored_Unused      at 48 range 0 .. 17 * 32 - 1;
      Base_Priority       at 48 range 0 .. 31;
      Task_Type_Name      at 52 range 0 .. 31;
      Creation_PC         at 56 range 0 .. 31;
      Parent_Task_ID      at 60 range 0 .. 31;
   end record;

   type DBGEXT_Control_Block_Access is access all DBGEXT_Control_Block;

   function DBGEXT (Control_Block : DBGEXT_Control_Block_Access)
     return System.Aux_DEC.Unsigned_Word;
   --  Exported to s-taprop.adb to avoid having a VMS specific s-tasdeb.ads
   pragma Convention (C, DBGEXT);
   pragma Export_Function (DBGEXT, "GNAT$DBGEXT");
   --  This routine is called by CMA when VMS DEBUG wants the Gnat RTL
   --  to give it some assistance (primarily when tasks are debugged).
   --
   --  The single parameter is an "external control block". On input to
   --  the Gnat RTL this control block determines the debugging function
   --  to be performed, and supplies parameters.  This routine cases on
   --  the function code, and calls the appropriate Gnat RTL routine,
   --  which returns values by modifying the external control block.

   procedure Announce_Event
      (Event_EVCB    : Unsigned_Longword;
       Print_Routine : Print_Routine_Type := Default_Print_Routine'Access);
   --  Announce the occurence of a DEBUG tasking event

   procedure Cleanup_Event (Event_EVCB : Unsigned_Longword);
   --  After DEBUG has processed an event that has signalled, the signaller
   --  must cleanup. Cleanup consists of freeing the event control block.

   procedure Disable_Event
      (Flags       : Bit_Array_32;
       Event_Value : Unsigned_Longword;
       Event_Code  : Unsigned_Longword;
       Status      : out Cond_Value_Type);
   --  Disable a DEBUG tasking event

   function DoAC (S : String) return Address;
   --  Convert a string to the address of an internal buffer containing
   --  the counted ASCII.

   procedure Enable_Event
      (Flags       : Bit_Array_32;
       Event_Value : Unsigned_Longword;
       Event_Code  : Unsigned_Longword;
       Status      : out Cond_Value_Type);
   --  Enable a requested DEBUG tasking event

   procedure Find_Event_By_Code
      (Event_Code  : Unsigned_Longword;
       Event_Entry : out Unsigned_Longword;
       Status      : out Cond_Value_Type);
   --  Convert an event code to the address of the event entry

   procedure Find_Event_By_Name
      (Event_Name  : Unsigned_Longword;
       Event_Entry : out Unsigned_Longword;
       Status      : out Cond_Value_Type);
   --  Find an event entry given the event name

   procedure List_Entry_Waiters
     (Task_Value      : Task_Id;
      Full_Display    : Boolean := False;
      Suppress_Header : Boolean := False;
      Print_Routine   : Print_Routine_Type := Default_Print_Routine'Access);
   --  List information about tasks waiting on an entry

   procedure Put (S : String);
   --  Display S on standard output

   procedure Put_Line (S : String := "");
   --  Display S on standard output with an additional line terminator

   procedure Show_Event
      (Print_Routine : Print_Routine_Type := Default_Print_Routine'Access);
   --  Show what events are available

   procedure Show_One_Task
     (Task_Value      : Task_Id;
      Full_Display    : Boolean := False;
      Suppress_Header : Boolean := False;
      Print_Routine   : Print_Routine_Type := Default_Print_Routine'Access);
   --  Display information about one task

   procedure Show_Rendezvous
     (Task_Value      : Task_Id;
      Ada_State       : AASCIC := Empty_Text;
      Full_Display    : Boolean := False;
      Suppress_Header : Boolean := False;
      Print_Routine   : Print_Routine_Type := Default_Print_Routine'Access);
   --  Display information about a task rendezvous

   procedure Trace_Output (Message_String : String);
   --  Call Put_Output if Trace_on ("VMS")

   procedure Write (Fd : Integer; S : String; Count : Integer);

   --------------------
   -- Announce_Event --
   --------------------

   procedure Announce_Event
      (Event_EVCB    : Unsigned_Longword;
       Print_Routine : Print_Routine_Type := Default_Print_Routine'Access)
   is
      EVCB : constant Ada_Event_Control_Block_Access := To_EVCB (Event_EVCB);

      Event_Kind : constant Event_Kind_Type :=
                     (if EVCB.Sub_Event /= 0
                      then Event_Kind_Type (EVCB.Sub_Event)
                      else Event_Kind_Type (EVCB.Code));

      TI : constant String := "   Task %TASK !UI is ";
      --  Announce prefix

   begin
      Trace_Output ("Announce called");

      case Event_Kind is
         when Debug_Event_Activating =>
            Print_Routine (Print_FAO, Print_Newline,
              To_UL (DoAC (TI & "about to begin its activation")),
              EVCB.Value);
         when Debug_Event_Exception_Terminated =>
            Print_Routine (Print_FAO, Print_Newline,
              To_UL (DoAC (TI & "terminating because of an exception")),
              EVCB.Value);
         when Debug_Event_Run =>
            Print_Routine (Print_FAO, Print_Newline,
              To_UL (DoAC (TI & "about to run")),
              EVCB.Value);
         when Debug_Event_Abort_Terminated =>
            Print_Routine (Print_FAO, Print_Newline,
              To_UL (DoAC (TI & "terminating because of abort")),
              EVCB.Value);
         when Debug_Event_Terminated =>
            Print_Routine (Print_FAO, Print_Newline,
              To_UL (DoAC (TI & "terminating normally")),
              EVCB.Value);
         when others => null;
      end case;
   end Announce_Event;

   -------------------
   -- Cleanup_Event --
   -------------------

   procedure Cleanup_Event (Event_EVCB  : Unsigned_Longword) is
      EVCB : Ada_Event_Control_Block_Access := To_EVCB (Event_EVCB);
   begin
      Free (EVCB);
   end Cleanup_Event;

   ------------------------
   -- Continue_All_Tasks --
   ------------------------

   procedure Continue_All_Tasks is
   begin
      null; --  VxWorks
   end Continue_All_Tasks;

   ------------
   -- DBGEXT --
   ------------

   function DBGEXT
     (Control_Block : DBGEXT_Control_Block_Access)
      return System.Aux_DEC.Unsigned_Word
   is
      Print_Routine : Print_Routine_Type := Default_Print_Routine'Access;
   begin
      Trace_Output ("DBGEXT called");

      if Control_Block.Print_Routine /= Address_Zero then
         Print_Routine := To_Print_Routine_Type (Control_Block.Print_Routine);
      end if;

      case Control_Block.Function_Code is

         --  Convert a task value to a task number.
         --  The output results are stored in the CONTROL_BLOCK.

         when K_CVT_VALUE_NUM =>
            Trace_Output ("DBGEXT param 1 - CVT Value to NUM");
            Control_Block.Task_Number :=
              Control_Block.Task_Value.Known_Tasks_Index + 1;
            Control_Block.Status := K_SUCCESS;
            Trace_Output ("Task Number: ");
            Trace_Output (Integer'Image (Control_Block.Task_Number));
            return SS_NORMAL;

         --  Convert a task number to a task value.
         --  The output results are stored in the CONTROL_BLOCK.

         when K_CVT_NUM_VALUE =>
            Trace_Output ("DBGEXT param 2 - CVT NUM to Value");
            Trace_Output ("Task Number: ");
            Trace_Output (Integer'Image (Control_Block.Task_Number));
            Control_Block.Task_Value :=
              Known_Tasks (Control_Block.Task_Number - 1);
            Control_Block.Status := K_SUCCESS;
            Trace_Output ("Task Value: ");
            Trace_Output (Unsigned_Longword'Image
              (To_UL (Control_Block.Task_Value)));
            return SS_NORMAL;

         --  Obtain the "next" task after a specified task.
         --  ??? To do: If specified check the PRIORITY, STATE, and HOLD
         --  fields to restrict the selection of the next task.
         --  The output results are stored in the CONTROL_BLOCK.

         when K_NEXT_TASK =>
            Trace_Output ("DBGEXT param 3 - Next Task");
            Trace_Output ("Task Value: ");
            Trace_Output (Unsigned_Longword'Image
              (To_UL (Control_Block.Task_Value)));

            if Control_Block.Task_Value = null then
               Control_Block.Task_Value := Known_Tasks (Known_Tasks'First);
            else
               Control_Block.Task_Value :=
                 Known_Tasks (Control_Block.Task_Value.Known_Tasks_Index + 1);
            end if;

            if Control_Block.Task_Value = null then
               Control_Block.Task_Value := Known_Tasks (Known_Tasks'First);
            end if;

            Control_Block.Status := K_SUCCESS;
            return SS_NORMAL;

         --  Display the state of a task. The FULL bit is checked to decide if
         --  a full or brief task display is desired. The output results are
         --  stored in the CONTROL_BLOCK.

         when K_SHOW_TASK =>
            Trace_Output ("DBGEXT param 4 - Show Task");

            if Control_Block.Task_Value = null then
               Control_Block.Status := K_TASK_NOT_EXIST;
            else
               Show_One_Task
                 (Control_Block.Task_Value,
                  Control_Block.Ada_Flags (V_Full_Display),
                  Control_Block.Ada_Flags (V_Suppress_Header),
                  Print_Routine);

               Control_Block.Status := K_SUCCESS;
            end if;

            return SS_NORMAL;

         --  Enable a requested DEBUG tasking event

         when K_ENABLE_EVENT =>
            Trace_Output ("DBGEXT param 17 - Enable Event");
            Enable_Event
              (Control_Block.Flags,
               Control_Block.Event_Value_or_Name,
               Control_Block.Event_Code_or_EVCB,
               Control_Block.Status);

            return SS_NORMAL;

         --  Disable a DEBUG tasking event

         when K_DISABLE_EVENT =>
            Trace_Output ("DBGEXT param 18 - Disable Event");
            Disable_Event
              (Control_Block.Flags,
               Control_Block.Event_Value_or_Name,
               Control_Block.Event_Code_or_EVCB,
               Control_Block.Status);

            return SS_NORMAL;

         --  Announce the occurence of a DEBUG tasking event

         when K_ANNOUNCE_EVENT =>
            Trace_Output ("DBGEXT param 19 - Announce Event");
            Announce_Event
              (Control_Block.Event_Code_or_EVCB,
               Print_Routine);

            Control_Block.Status := K_SUCCESS;
            return SS_NORMAL;

         --  After DEBUG has processed an event that has signalled,
         --  the signaller must cleanup.
         --  Cleanup consists of freeing the event control block.

         when K_CLEANUP_EVENT =>
            Trace_Output ("DBGEXT param 24 - Cleanup Event");
            Cleanup_Event (Control_Block.Event_Code_or_EVCB);

            Control_Block.Status := K_SUCCESS;
            return SS_NORMAL;

         --  Show what events are available

         when K_SHOW_EVENT_DEF =>
            Trace_Output ("DBGEXT param 25 - Show Event Def");
            Show_Event (Print_Routine);

            Control_Block.Status := K_SUCCESS;
            return SS_NORMAL;

         --  Convert an event code to the address of the event entry

         when K_FIND_EVENT_BY_CODE =>
            Trace_Output ("DBGEXT param 29 - Find Event by Code");
            Find_Event_By_Code
              (Control_Block.Event_Code_or_EVCB,
               Control_Block.Event_Entry,
               Control_Block.Status);

            return SS_NORMAL;

         --  Find an event entry given the event name

         when K_FIND_EVENT_BY_NAME =>
            Trace_Output ("DBGEXT param 30 - Find Event by Name");
            Find_Event_By_Name
              (Control_Block.Event_Value_or_Name,
               Control_Block.Event_Entry,
               Control_Block.Status);
            return SS_NORMAL;

         --  ??? To do: Implement priority events
         --  Get, set or restore a task's priority

         when K_GET_PRIORITY or K_SET_PRIORITY or K_RESTORE_PRIORITY =>
            Trace_Output ("DBGEXT priority param - Not yet implemented");
            Trace_Output (Function_Codes'Image
             (Control_Block.Function_Code));
            return SS_BADPARAM;

         --  ??? To do: Implement show statistics event
         --  Display task statistics

         when K_SHOW_STAT =>
            Trace_Output ("DBGEXT show stat param - Not yet implemented");
            Trace_Output (Function_Codes'Image
             (Control_Block.Function_Code));
            return SS_BADPARAM;

         --  ??? To do: Implement get caller event
         --  Obtain the caller of a task in a rendezvous. If no rendezvous,
         --  null is returned

         when K_GET_CALLER =>
            Trace_Output ("DBGEXT get caller param - Not yet implemented");
            Trace_Output (Function_Codes'Image
             (Control_Block.Function_Code));
            return SS_BADPARAM;

         --  ??? To do: Implement set terminate event
         --  Terminate a task

         when K_SET_ABORT =>
            Trace_Output ("DBGEXT set terminate param - Not yet implemented");
            Trace_Output (Function_Codes'Image
             (Control_Block.Function_Code));
            return SS_BADPARAM;

         --  ??? To do: Implement show deadlock event
         --  Detect a deadlock

         when K_SHOW_DEADLOCK =>
            Trace_Output ("DBGEXT show deadlock param - Not yet implemented");
            Trace_Output (Function_Codes'Image
             (Control_Block.Function_Code));
            return SS_BADPARAM;

         when others =>
            Trace_Output ("DBGEXT bad param: ");
            Trace_Output (Function_Codes'Image
             (Control_Block.Function_Code));
            return SS_BADPARAM;

      end case;
   end DBGEXT;

   ---------------------------
   -- Default_Print_Routine --
   ---------------------------

   procedure Default_Print_Routine
     (Print_Function    : Print_Functions;
      Print_Subfunction : Print_Functions;
      P1                : Unsigned_Longword := 0;
      P2                : Unsigned_Longword := 0;
      P3                : Unsigned_Longword := 0;
      P4                : Unsigned_Longword := 0;
      P5                : Unsigned_Longword := 0;
      P6                : Unsigned_Longword := 0)
   is
      Status    : Cond_Value_Type;
      Linlen    : Unsigned_Word;
      Item_List : Unsigned_Longword_Array (1 .. 17) :=
        (1 .. 17 => 0);
   begin

      case Print_Function is
         when Print_Control | Print_String =>
            null;

         --  Formatted Ascii Output

         when Print_FAO =>
            Item_List (1) := P2;
            Item_List (2) := P3;
            Item_List (3) := P4;
            Item_List (4) := P5;
            Item_List (5) := P6;
            FAOL
              (Status,
               To_AASCIC (P1).Text,
               Linlen,
               Print_Routine_Linbuf
                 (1 + Print_Routine_Bufcnt .. Print_Routine_Bufsiz),
               Item_List);

            Print_Routine_Bufcnt := Print_Routine_Bufcnt + Integer (Linlen);

         --  Symbolic output

         when Print_Symbol =>
            Item_List (1) := P1;
            FAOL
              (Status,
               "!XI",
               Linlen,
               Print_Routine_Linbuf
                 (1 + Print_Routine_Bufcnt .. Print_Routine_Bufsiz),
               Item_List);

            Print_Routine_Bufcnt := Print_Routine_Bufcnt + Integer (Linlen);

         when others =>
            null;
      end case;

      case Print_Subfunction is

         --  Output buffer with a terminating newline

         when Print_Newline =>
            Put_Output (Status,
              Print_Routine_Linbuf (1 .. Print_Routine_Bufcnt));
            Print_Routine_Bufcnt := 0;

         --  Buffer the output

         when No_Print =>
            null;

         when others =>
            null;
      end case;

   end Default_Print_Routine;

   -------------------
   -- Disable_Event --
   -------------------

   procedure Disable_Event
      (Flags       : Bit_Array_32;
       Event_Value : Unsigned_Longword;
       Event_Code  : Unsigned_Longword;
       Status      : out Cond_Value_Type)
   is
      Task_Value : Task_Id;
      Task_Index : constant Integer := Integer (Event_Value) - 1;
   begin

      Events_Enabled_Count := Events_Enabled_Count - 1;

      if Flags (V_EVNT_ALL) then
         Global_Task_Debug_Events (Integer (Event_Code)) := False;
         Status := K_SUCCESS;
      else
         if Task_Index in Known_Tasks'Range then
            Task_Value := Known_Tasks (Task_Index);
            if Task_Value /= null then
               Task_Value.Common.Debug_Events (Integer (Event_Code)) := False;
               Status := K_SUCCESS;
            else
               Status := K_TASK_NOT_EXIST;
            end if;
         else
            Status := K_TASK_NOT_EXIST;
         end if;
      end if;

      --  Keep count of events for efficiency

      if Events_Enabled_Count <= 0 then
         Events_Enabled_Count := 0;
         Global_Task_Debug_Event_Set := False;
      end if;

   end Disable_Event;

   ----------
   -- DoAC --
   ----------

   function DoAC (S : String) return Address is
   begin
      AC_Buffer.Count := S'Length;
      AC_Buffer.Text (1 .. AC_Buffer.Count) := S;
      return AC_Buffer'Address;
   end DoAC;

   ------------------
   -- Enable_Event --
   ------------------

   procedure Enable_Event
      (Flags       : Bit_Array_32;
       Event_Value : Unsigned_Longword;
       Event_Code  : Unsigned_Longword;
       Status      : out Cond_Value_Type)
   is
      Task_Value : Task_Id;
      Task_Index : constant Integer := Integer (Event_Value) - 1;
   begin

      --  At least one event enabled, any and all events will cause a
      --  condition to be raised and checked. Major tasking slowdown!

      Global_Task_Debug_Event_Set := True;
      Events_Enabled_Count := Events_Enabled_Count + 1;

      if Flags (V_EVNT_ALL) then
         Global_Task_Debug_Events (Integer (Event_Code)) := True;
         Status := K_SUCCESS;
      else
         if Task_Index in Known_Tasks'Range then
            Task_Value := Known_Tasks (Task_Index);
            if Task_Value /= null then
               Task_Value.Common.Debug_Events (Integer (Event_Code)) := True;
               Status := K_SUCCESS;
            else
               Status := K_TASK_NOT_EXIST;
            end if;
         else
            Status := K_TASK_NOT_EXIST;
         end if;
      end if;

   end Enable_Event;

   ------------------------
   -- Find_Event_By_Code --
   ------------------------

   procedure Find_Event_By_Code
      (Event_Code  : Unsigned_Longword;
       Event_Entry : out Unsigned_Longword;
       Status      : out Cond_Value_Type)
   is
      K_SUCCESS        : constant := 1;
      K_NO_SUCH_EVENT  : constant := 9;

   begin
      Trace_Output ("Looking for Event: ");
      Trace_Output (Unsigned_Longword'Image (Event_Code));

      for I in Event_Kind_Type'Range loop
         if Event_Code = Unsigned_Longword (Event_Directory (I).Code) then
            Event_Entry := To_UL (Event_Directory (I)'Address);
            Trace_Output ("Found Event # ");
            Trace_Output (Integer'Image (I));
            Status := K_SUCCESS;
            return;
         end if;
      end loop;

      Status := K_NO_SUCH_EVENT;
   end Find_Event_By_Code;

   ------------------------
   -- Find_Event_By_Name --
   ------------------------

   procedure Find_Event_By_Name
      (Event_Name  : Unsigned_Longword;
       Event_Entry : out Unsigned_Longword;
       Status      : out Cond_Value_Type)
   is
      K_SUCCESS        : constant := 1;
      K_NO_SUCH_EVENT  : constant := 9;

      Event_Name_Cstr : constant ASCIC := To_AASCIC (Event_Name).all;
   begin
      Trace_Output ("Looking for Event: ");
      Trace_Output (Event_Name_Cstr.Text);

      for I in Event_Kind_Type'Range loop
         if Event_Name_Cstr.Count >= Event_Directory (I).Minchr
            and then Event_Name_Cstr.Count <= Event_Directory (I).Name.Count
            and then Event_Name_Cstr.Text (1 .. Event_Directory (I).Minchr) =
                Event_Directory (I).Name.Text (1 .. Event_Directory (I).Minchr)
         then
            Event_Entry := To_UL (Event_Directory (I)'Address);
            Trace_Output ("Found Event # ");
            Trace_Output (Integer'Image (I));
            Status := K_SUCCESS;
            return;
         end if;
      end loop;

      Status := K_NO_SUCH_EVENT;
   end Find_Event_By_Name;

   --------------------
   -- Get_User_State --
   --------------------

   function Get_User_State return Long_Integer is
   begin
      return STPO.Self.User_State;
   end Get_User_State;

   ------------------------
   -- List_Entry_Waiters --
   ------------------------

   procedure List_Entry_Waiters
     (Task_Value      : Task_Id;
      Full_Display    : Boolean := False;
      Suppress_Header : Boolean := False;
      Print_Routine   : Print_Routine_Type := Default_Print_Routine'Access)
   is
      pragma Unreferenced (Suppress_Header);

      Entry_Call : Entry_Call_Link;
      Have_Some  : Boolean := False;
   begin
      if not Full_Display then
         return;
      end if;

      if Task_Value.Entry_Queues'Length > 0 then
         Print_Routine (Print_FAO, Print_Newline,
           To_UL (DoAC ("        Waiting entry callers:")));
      end if;
      for I in Task_Value.Entry_Queues'Range loop
         Entry_Call := Task_Value.Entry_Queues (I).Head;
         if Entry_Call /= null then
            Have_Some := True;

            Print_Routine (Print_FAO, Print_Newline,
              To_UL (DoAC ("          Waiters for entry !UI:")),
              To_UL (I));

            loop
               declare
                  Task_Image : ASCIC :=
                   (Entry_Call.Self.Common.Task_Image_Len,
                    Entry_Call.Self.Common.Task_Image
                     (1 .. Entry_Call.Self.Common.Task_Image_Len));
               begin
                  Print_Routine (Print_FAO, Print_Newline,
                    To_UL (DoAC ("              %TASK !UI, type: !AC")),
                    To_UL (Entry_Call.Self.Known_Tasks_Index + 1),
                    To_UL (Task_Image'Address));
                  if Entry_Call = Task_Value.Entry_Queues (I).Tail then
                     exit;
                  end if;
                  Entry_Call := Entry_Call.Next;
               end;
            end loop;
         end if;
      end loop;
      if not Have_Some then
         Print_Routine (Print_FAO, Print_Newline,
           To_UL (DoAC ("          none.")));
      end if;
   end List_Entry_Waiters;

   ----------------
   -- List_Tasks --
   ----------------

   procedure List_Tasks is
      C : Task_Id;
   begin
      C := All_Tasks_List;

      while C /= null loop
         Print_Task_Info (C);
         C := C.Common.All_Tasks_Link;
      end loop;
   end List_Tasks;

   ------------------------
   -- Print_Current_Task --
   ------------------------

   procedure Print_Current_Task is
   begin
      Print_Task_Info (STPO.Self);
   end Print_Current_Task;

   ---------------------
   -- Print_Task_Info --
   ---------------------

   procedure Print_Task_Info (T : Task_Id) is
      Entry_Call : Entry_Call_Link;
      Parent     : Task_Id;

   begin
      if T = null then
         Put_Line ("null task");
         return;
      end if;

      Put (T.Common.Task_Image (1 .. T.Common.Task_Image_Len) & ": " &
           Task_States'Image (T.Common.State));

      Parent := T.Common.Parent;

      if Parent = null then
         Put (", parent: <none>");
      else
         Put (", parent: " &
              Parent.Common.Task_Image (1 .. Parent.Common.Task_Image_Len));
      end if;

      Put (", prio:" & T.Common.Current_Priority'Img);

      if not T.Callable then
         Put (", not callable");
      end if;

      if T.Aborting then
         Put (", aborting");
      end if;

      if T.Deferral_Level /= 0 then
         Put (", abort deferred");
      end if;

      if T.Common.Call /= null then
         Entry_Call := T.Common.Call;
         Put (", serving:");

         while Entry_Call /= null loop
            Put (To_Integer (Entry_Call.Self)'Img);
            Entry_Call := Entry_Call.Acceptor_Prev_Call;
         end loop;
      end if;

      if T.Open_Accepts /= null then
         Put (", accepting:");

         for J in T.Open_Accepts'Range loop
            Put (T.Open_Accepts (J).S'Img);
         end loop;

         if T.Terminate_Alternative then
            Put (" or terminate");
         end if;
      end if;

      if T.User_State /= 0 then
         Put (", state:" & T.User_State'Img);
      end if;

      Put_Line;
   end Print_Task_Info;

   ---------
   -- Put --
   ---------

   procedure Put (S : String) is
   begin
      Write (2, S, S'Length);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String := "") is
   begin
      Write (2, S & ASCII.LF, S'Length + 1);
   end Put_Line;

   ----------------------
   -- Resume_All_Tasks --
   ----------------------

   procedure Resume_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      pragma Unreferenced (Thread_Self);
   begin
      null; --  VxWorks
   end Resume_All_Tasks;

   ---------------
   -- Set_Trace --
   ---------------

   procedure Set_Trace (Flag  : Character; Value : Boolean := True) is
   begin
      Trace_On (Flag) := Value;
   end Set_Trace;

   --------------------
   -- Set_User_State --
   --------------------

   procedure Set_User_State (Value : Long_Integer) is
   begin
      STPO.Self.User_State := Value;
   end Set_User_State;

   ----------------
   -- Show_Event --
   ----------------

   procedure Show_Event
      (Print_Routine : Print_Routine_Type := Default_Print_Routine'Access)
   is
   begin
      for I in Event_Def_Help'Range loop
         Print_Routine (Print_FAO, Print_Newline, To_UL (Event_Def_Help (I)));
      end loop;

      for I in Event_Kind_Type'Range loop
         Print_Routine (Print_FAO, Print_Newline,
           To_UL (Event_Directory
                   (Global_Event_Display_Order (I)).Name'Address));
         Print_Routine (Print_FAO, Print_Newline,
           To_UL (Event_Directory (Global_Event_Display_Order (I)).Help));
      end loop;
   end Show_Event;

   --------------------
   -- Show_One_Task --
   --------------------

   procedure Show_One_Task
     (Task_Value      : Task_Id;
      Full_Display    : Boolean := False;
      Suppress_Header : Boolean := False;
      Print_Routine   : Print_Routine_Type := Default_Print_Routine'Access)
   is
      Task_SP            : System.Address := Address_Zero;
      Stack_Base         : System.Address := Address_Zero;
      Stack_Top          : System.Address := Address_Zero;
      TCB_Size           : Unsigned_Longword := 0;
      CMA_TCB_Size       : Unsigned_Longword := 0;
      Stack_Guard_Size   : Unsigned_Longword := 0;
      Total_Task_Storage : Unsigned_Longword := 0;
      Stack_In_Use       : Unsigned_Longword := 0;
      Reserved_Size      : Unsigned_Longword := 0;
      Hold_Flag          : Unsigned_Longword := 0;
      Sched_State        : Unsigned_Longword := 0;
      User_Prio          : Unsigned_Longword := 0;
      Stack_Size         : Unsigned_Longword := 0;
      Run_State          : Boolean := False;
      Rea_State          : Boolean := False;
      Sus_State          : Boolean := False;
      Ter_State          : Boolean := False;

      Current_Flag : AASCIC := NoStar;
      Hold_String  : AASCIC := NoHold;
      Ada_State    : AASCIC := Ada_State_Invalid_State;
      Debug_State  : AASCIC := Debug_State_Emp;

      Ada_State_Len   : constant Unsigned_Longword := 17;
      Debug_State_Len : constant Unsigned_Longword := 5;

      Entry_Call : Entry_Call_Record;

   begin

      --  Initialize local task info variables

      Task_SP := Address_Zero;
      Stack_Base := Address_Zero;
      Stack_Top := Address_Zero;
      CMA_TCB_Size := 0;
      Stack_Guard_Size := 0;
      Reserved_Size := 0;
      Hold_Flag := 0;
      Sched_State := 0;
      TCB_Size := Unsigned_Longword (Task_Id'Size);

      if not Suppress_Header or else Full_Display then
         Print_Routine (Print_FAO, Print_Newline, To_UL (Empty_Text));
         Print_Routine (Print_FAO, Print_Newline, To_UL (Header));
      end if;

      Trace_Output ("Show_One_Task Task Value: ");
      Trace_Output (Unsigned_Longword'Image (To_UL (Task_Value)));

      --  Callback to DEBUG to get some task info

      if Task_Value.Common.State /= Terminated then
         Debug_Get
           (STPO.Get_Thread_Id (Task_Value),
            CMA_C_DEBGET_STACKPTR,
            Task_SP,
            8);

         Debug_Get
           (STPO.Get_Thread_Id (Task_Value),
            CMA_C_DEBGET_TCB_SIZE,
            CMA_TCB_Size,
            4);

         Debug_Get
           (STPO.Get_Thread_Id (Task_Value),
            CMA_C_DEBGET_GUARDSIZE,
            Stack_Guard_Size,
            4);

         Debug_Get
           (STPO.Get_Thread_Id (Task_Value),
            CMA_C_DEBGET_YELLOWSIZE,
            Reserved_Size,
            4);

         Debug_Get
           (STPO.Get_Thread_Id (Task_Value),
            CMA_C_DEBGET_STACK_BASE,
            Stack_Base,
            8);

         Debug_Get
           (STPO.Get_Thread_Id (Task_Value),
            CMA_C_DEBGET_STACK_TOP,
            Stack_Top,
            8);

         Stack_Size := Unsigned_Longword (Stack_Base - Stack_Top)
           - Reserved_Size - Stack_Guard_Size;
         Stack_In_Use := Unsigned_Longword (Stack_Base - Task_SP) + 4;
         Total_Task_Storage := TCB_Size + Stack_Size + Stack_Guard_Size
           + Reserved_Size + CMA_TCB_Size;

         Debug_Get
           (STPO.Get_Thread_Id (Task_Value),
            CMA_C_DEBGET_IS_HELD,
            Hold_Flag,
            4);

         Hold_String := (if Hold_Flag /= 0 then Hold else NoHold);

         Debug_Get
           (STPO.Get_Thread_Id (Task_Value),
            CMA_C_DEBGET_SCHED_STATE,
            Sched_State,
            4);
      end if;

      Run_State := False;
      Rea_State := False;
      Sus_State := Task_Value.Common.State = Unactivated;
      Ter_State := Task_Value.Common.State = Terminated;

      if not Ter_State then
         Run_State := Sched_State = 0;
         Rea_State := Sched_State = 1;
         Sus_State := Sched_State /= 0 and Sched_State /= 1;
      end if;

      --  Set the debug state

      if Run_State then
         Debug_State := Debug_State_Run;
      elsif Rea_State then
         Debug_State := Debug_State_Rea;
      elsif Sus_State then
         Debug_State := Debug_State_Sus;
      elsif Ter_State then
         Debug_State := Debug_State_Ter;
      end if;

      Trace_Output ("Before case State: ");
      Trace_Output (Task_States'Image (Task_Value.Common.State));

      --  Set the Ada state

      case Task_Value.Common.State is
         when Unactivated =>
            Ada_State := Ada_State_Not_Yet_Activated;

         when Activating =>
            Ada_State := Ada_State_Activating;

         when Runnable =>
            Ada_State := Ada_State_Runnable;

         when Terminated =>
            Ada_State := Ada_State_Terminated;

         when Activator_Sleep =>
            Ada_State := Ada_State_Activating_Tasks;

         when Acceptor_Sleep =>
            Ada_State := Ada_State_Accept;

         when Acceptor_Delay_Sleep =>
            Ada_State := Ada_State_Select_or_Delay;

         when Entry_Caller_Sleep =>
            Entry_Call :=
              Task_Value.Entry_Calls (Task_Value.ATC_Nesting_Level);

            case Entry_Call.Mode is
               when Simple_Call =>
                  Ada_State := Ada_State_Entry_Call;
               when Conditional_Call =>
                  Ada_State := Ada_State_Cond_Entry_Call;
               when Timed_Call =>
                  Ada_State := Ada_State_Timed_Entry_Call;
               when Asynchronous_Call =>
                  Ada_State := Ada_State_Async_Entry_Call;
            end case;

         when Async_Select_Sleep =>
            Ada_State := Ada_State_Select_or_Abort;

         when Delay_Sleep =>
            Ada_State := Ada_State_Delay;

         when Master_Completion_Sleep =>
            Ada_State := Ada_State_Completed;

         when Master_Phase_2_Sleep =>
            Ada_State := Ada_State_Completed;

         when Interrupt_Server_Idle_Sleep |
              Interrupt_Server_Blocked_Interrupt_Sleep |
              Timer_Server_Sleep |
              Interrupt_Server_Blocked_On_Event_Flag =>
            Ada_State := Ada_State_Server;

         when AST_Server_Sleep =>
            Ada_State := Ada_State_IO_or_AST;

         when Asynchronous_Hold =>
            Ada_State := Ada_State_Async_Hold;

      end case;

      if Task_Value.Terminate_Alternative then
         Ada_State := Ada_State_Select_or_Term;
      end if;

      if Task_Value.Aborting then
         Ada_State := Ada_State_Aborting;
      end if;

      User_Prio := To_UL (Task_Value.Common.Current_Priority);
      Trace_Output ("After user_prio");

      --  Flag the current task

      Current_Flag := (if Task_Value = Self then Star else NoStar);

      --  Show task info

      Print_Routine (Print_FAO, No_Print, To_UL (DoAC ("!AC%TASK !5<!UI!>")),
        To_UL (Current_Flag), To_UL (Task_Value.Known_Tasks_Index + 1));

      Print_Routine (Print_FAO, No_Print, To_UL (DoAC ("!2UB")), User_Prio);

      Print_Routine (Print_FAO, No_Print, To_UL (DoAC (" !AC !5AD !17AD ")),
        To_UL (Hold_String), Debug_State_Len, To_UL (Debug_State),
        Ada_State_Len, To_UL (Ada_State));

--      Print_Routine (Print_Symbol, Print_Newline,
--         Fetch (To_Addr (Task_Value.Common.Task_Entry_Point)));

      Print_Routine (Print_FAO, Print_Newline, To_UL (Empty_Text));

      --  If /full qualfier passed, show detailed info

      if Full_Display then
         Show_Rendezvous (Task_Value, Ada_State, Full_Display,
           Suppress_Header, Print_Routine);

         List_Entry_Waiters (Task_Value, Full_Display,
           Suppress_Header, Print_Routine);

         Print_Routine (Print_FAO, Print_Newline, To_UL (Empty_Text));

         declare
            Task_Image : ASCIC := (Task_Value.Common.Task_Image_Len,
              Task_Value.Common.Task_Image
               (1 .. Task_Value.Common.Task_Image_Len));
         begin
            Print_Routine (Print_FAO, Print_Newline,
              To_UL (DoAC ("        Task type:      !AC")),
              To_UL (Task_Image'Address));
         end;

         --  How to find Creation_PC ???
--         Print_Routine (Print_FAO, No_Print,
--           To_UL (DoAC ("        Created at PC:  ")),
--         Print_Routine (Print_FAO, Print_Newline, Creation_PC);

         if Task_Value.Common.Parent /= null then
            Print_Routine (Print_FAO, Print_Newline,
              To_UL (DoAC ("        Parent task:    %TASK !UI")),
              To_UL (Task_Value.Common.Parent.Known_Tasks_Index + 1));
         else
            Print_Routine (Print_FAO, Print_Newline,
             To_UL (DoAC ("        Parent task:    none")));
         end if;

--         Print_Routine (Print_FAO, No_Print,
--           To_UL (DoAC ("        Start PC:       ")));
--         Print_Routine (Print_Symbol, Print_Newline,
--            Fetch (To_Addr (Task_Value.Common.Task_Entry_Point)));

         Print_Routine (Print_FAO, Print_Newline,
          To_UL (DoAC (
           "        Task control block:             Stack storage (bytes):")));

         Print_Routine (Print_FAO, Print_Newline,
          To_UL (DoAC (
           "          Task value:   !10<!UI!>        RESERVED_BYTES:  !10UI")),
          To_UL (Task_Value), Reserved_Size);

         Print_Routine (Print_FAO, Print_Newline,
          To_UL (DoAC (
           "          Entries:      !10<!UI!>        TOP_GUARD_SIZE:  !10UI")),
          To_UL (Task_Value.Entry_Num), Stack_Guard_Size);

         Print_Routine (Print_FAO, Print_Newline,
          To_UL (DoAC (
           "          Size:         !10<!UI!>        STORAGE_SIZE:    !10UI")),
          TCB_Size + CMA_TCB_Size, Stack_Size);

         Print_Routine (Print_FAO, Print_Newline,
          To_UL (DoAC (
           "        Stack addresses:                 Bytes in use:    !10UI")),
          Stack_In_Use);

         Print_Routine (Print_FAO, Print_Newline,
          To_UL (DoAC ("          Top address:  !10<!XI!>")),
          To_UL (Stack_Top));

         Print_Routine (Print_FAO, Print_Newline,
          To_UL (DoAC (
           "          Base address: !10<!XI!>      Total storage:     !10UI")),
          To_UL (Stack_Base), Total_Task_Storage);
      end if;

   end Show_One_Task;

   ---------------------
   -- Show_Rendezvous --
   ---------------------

   procedure Show_Rendezvous
     (Task_Value      : Task_Id;
      Ada_State       : AASCIC := Empty_Text;
      Full_Display    : Boolean := False;
      Suppress_Header : Boolean := False;
      Print_Routine   : Print_Routine_Type := Default_Print_Routine'Access)
   is
      pragma Unreferenced (Ada_State);
      pragma Unreferenced (Suppress_Header);

      Temp_Entry  : Entry_Index;
      Entry_Call  : Entry_Call_Record;
      Called_Task : Task_Id;
      AWR         : constant String := "        Awaiting rendezvous at: ";
      --  Common prefix

      procedure Print_Accepts;
      --  Display information about task rendezvous accepts

      procedure Print_Accepts is
      begin
         if Task_Value.Open_Accepts /= null then
            for I in Task_Value.Open_Accepts'Range loop
               Temp_Entry := Entry_Index (Task_Value.Open_Accepts (I).S);
               declare
                  Entry_Name_Image : ASCIC :=
                    (Task_Value.Entry_Names (Temp_Entry).all'Length,
                     Task_Value.Entry_Names (Temp_Entry).all);
               begin
                  Trace_Output ("Accept at: " & Entry_Name_Image.Text);
                  Print_Routine (Print_FAO, Print_Newline,
                    To_UL (DoAC ("             accept at: !AC")),
                    To_UL (Entry_Name_Image'Address));
               end;
            end loop;
         end if;
      end Print_Accepts;
   begin
      if not Full_Display then
         return;
      end if;

      Trace_Output ("Show_Rendezvous Task Value: ");
      Trace_Output (Unsigned_Longword'Image (To_UL (Task_Value)));

      if Task_Value.Common.State = Acceptor_Sleep and then
         not Task_Value.Terminate_Alternative
      then
         if Task_Value.Open_Accepts /= null then
            Temp_Entry := Entry_Index (Task_Value.Open_Accepts
              (Task_Value.Open_Accepts'First).S);
            declare
               Entry_Name_Image : ASCIC :=
                 (Task_Value.Entry_Names (Temp_Entry).all'Length,
                  Task_Value.Entry_Names (Temp_Entry).all);
            begin
               Trace_Output (AWR & "accept " & Entry_Name_Image.Text);
               Print_Routine (Print_FAO, Print_Newline,
                 To_UL (DoAC (AWR & "accept !AC")),
                 To_UL (Entry_Name_Image'Address));
            end;

         else
            Print_Routine (Print_FAO, Print_Newline,
              To_UL (DoAC ("        entry name unavailable")));
         end if;
      else
         case Task_Value.Common.State is
            when Acceptor_Sleep =>
               Print_Routine (Print_FAO, Print_Newline,
                 To_UL (DoAC (AWR & "select with terminate.")));
               Print_Accepts;

            when Async_Select_Sleep =>
               Print_Routine (Print_FAO, Print_Newline,
                 To_UL (DoAC (AWR & "select.")));
               Print_Accepts;

            when Acceptor_Delay_Sleep =>
               Print_Routine (Print_FAO, Print_Newline,
                 To_UL (DoAC (AWR & "select with delay.")));
               Print_Accepts;

            when Entry_Caller_Sleep =>
               Entry_Call :=
                 Task_Value.Entry_Calls (Task_Value.ATC_Nesting_Level);

               case Entry_Call.Mode is
                  when Simple_Call =>
                     Print_Routine (Print_FAO, Print_Newline,
                       To_UL (DoAC (AWR & "entry call")));
                  when Conditional_Call =>
                     Print_Routine (Print_FAO, Print_Newline,
                       To_UL (DoAC (AWR & "entry call with else")));
                  when Timed_Call =>
                     Print_Routine (Print_FAO, Print_Newline,
                       To_UL (DoAC (AWR & "entry call with delay")));
                  when Asynchronous_Call =>
                     Print_Routine (Print_FAO, Print_Newline,
                        To_UL (DoAC (AWR & "entry call with abort")));
               end case;
               Called_Task := Entry_Call.Called_Task;
               declare
                  Task_Image : ASCIC := (Called_Task.Common.Task_Image_Len,
                    Called_Task.Common.Task_Image
                     (1 .. Called_Task.Common.Task_Image_Len));
                  Entry_Name_Image : ASCIC :=
                    (Called_Task.Entry_Names (Entry_Call.E).all'Length,
                     Called_Task.Entry_Names (Entry_Call.E).all);
               begin
                  Print_Routine (Print_FAO, Print_Newline,
                    To_UL (DoAC
                     ("        for entry !AC in %TASK !UI type !AC")),
                    To_UL (Entry_Name_Image'Address),
                    To_UL (Called_Task.Known_Tasks_Index),
                    To_UL (Task_Image'Address));
               end;

            when others =>
               return;
         end case;
      end if;

   end Show_Rendezvous;

   ------------------------
   -- Signal_Debug_Event --
   ------------------------

   procedure Signal_Debug_Event
    (Event_Kind : Event_Kind_Type; Task_Value : Task_Id)
   is
      Do_Signal : Boolean;
      EVCB      : Ada_Event_Control_Block_Access;

      EVCB_Sent    : constant := 16#9B#;
      Ada_Facility : constant := 49;
      SS_DBGEVENT  : constant := 1729;
   begin
      Do_Signal := Global_Task_Debug_Events (Event_Kind);

      if not Do_Signal then
         if Task_Value /= null then
            Do_Signal := Do_Signal
              or else Task_Value.Common.Debug_Events (Event_Kind);
         end if;
      end if;

      if Do_Signal then
         --  Build an a tasking event control block and signal DEBUG

         EVCB := new Ada_Event_Control_Block_Type;
         EVCB.Code := Unsigned_Word (Event_Kind);
         EVCB.Sentinal := EVCB_Sent;
         EVCB.Facility := Ada_Facility;

         if Task_Value /= null then
            EVCB.Value := Unsigned_Longword (Task_Value.Known_Tasks_Index + 1);
         else
            EVCB.Value := 0;
         end if;

         EVCB.Sub_Event := 0;
         EVCB.P1 := 0;
         EVCB.Sigargs := 0;
         EVCB.Flags := 0;
         EVCB.Unused1 := 0;
         EVCB.Unused2 := 0;

         Signal (SS_DBGEVENT, 1, To_UL (EVCB));
      end if;
   end Signal_Debug_Event;

   --------------------
   -- Stop_All_Tasks --
   --------------------

   procedure Stop_All_Tasks is
   begin
      null; --  VxWorks
   end Stop_All_Tasks;

   ----------------------------
   -- Stop_All_Tasks_Handler --
   ----------------------------

   procedure Stop_All_Tasks_Handler is
   begin
      null; --  VxWorks
   end Stop_All_Tasks_Handler;

   -----------------------
   -- Suspend_All_Tasks --
   -----------------------

   procedure Suspend_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      pragma Unreferenced (Thread_Self);
   begin
      null; --  VxWorks
   end Suspend_All_Tasks;

   ------------------------
   -- Task_Creation_Hook --
   ------------------------

   procedure Task_Creation_Hook (Thread : OS_Interface.Thread_Id) is
      pragma Unreferenced (Thread);
   begin
      null; --  VxWorks
   end Task_Creation_Hook;

   ---------------------------
   -- Task_Termination_Hook --
   ---------------------------

   procedure Task_Termination_Hook is
   begin
      null; --  VxWorks
   end Task_Termination_Hook;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Self_Id  : Task_Id;
      Msg      : String;
      Flag     : Character;
      Other_Id : Task_Id := null)
   is
   begin
      if Trace_On (Flag) then
         Put (To_Integer (Self_Id)'Img &
              ':' & Flag & ':' &
              Self_Id.Common.Task_Image (1 .. Self_Id.Common.Task_Image_Len) &
              ':');

         if Other_Id /= null then
            Put (To_Integer (Other_Id)'Img & ':');
         end if;

         Put_Line (Msg);
      end if;
   end Trace;

   ------------------
   -- Trace_Output --
   ------------------

   procedure Trace_Output (Message_String : String) is
   begin
      if Trace_On ('V') and Trace_On ('M') and Trace_On ('S') then
         Put_Output (Message_String);
      end if;
   end Trace_Output;

   -----------
   -- Write --
   -----------

   procedure Write (Fd : Integer; S : String; Count : Integer) is
      Discard : System.CRTL.ssize_t;
      pragma Unreferenced (Discard);
   begin
      Discard := System.CRTL.write (Fd, S (S'First)'Address,
                                    System.CRTL.size_t (Count));
      --  Is it really right to ignore write errors here ???
   end Write;

end System.Tasking.Debug;
