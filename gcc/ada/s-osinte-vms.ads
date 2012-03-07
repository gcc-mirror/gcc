------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--          Copyright (C) 1995-2012, Free Software Foundation, Inc.         --
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

--  This is the OpenVMS version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by the tasking run-time (libgnarl).

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Interfaces.C;

with Ada.Unchecked_Conversion;

with System.Aux_DEC;

package System.OS_Interface is
   pragma Preelaborate;

   --  pragma Linker_Options ("--for-linker=/threads_enable");
   --  Enable upcalls and multiple kernel threads.

   subtype int            is Interfaces.C.int;
   subtype short          is Interfaces.C.short;
   subtype long           is Interfaces.C.long;
   subtype unsigned       is Interfaces.C.unsigned;
   subtype unsigned_short is Interfaces.C.unsigned_short;
   subtype unsigned_long  is Interfaces.C.unsigned_long;
   subtype unsigned_char  is Interfaces.C.unsigned_char;
   subtype plain_char     is Interfaces.C.plain_char;
   subtype size_t         is Interfaces.C.size_t;

   -----------------------------
   -- Signals (Interrupt IDs) --
   -----------------------------

   --  Type signal has an arbitrary limit of 31

   Max_Interrupt : constant := 31;
   type Signal is new unsigned range 0 .. Max_Interrupt;
   for Signal'Size use unsigned'Size;

   type sigset_t is array (Signal) of Boolean;
   pragma Pack (sigset_t);

   --  Interrupt_Number_Type
   --  Unsigned long integer denoting the number of an interrupt

   subtype Interrupt_Number_Type is unsigned_long;

   --  OpenVMS system services return values of type Cond_Value_Type

   subtype Cond_Value_Type is unsigned_long;
   subtype Short_Cond_Value_Type is unsigned_short;

   type IO_Status_Block_Type is record
      Status   : Short_Cond_Value_Type;
      Count    : unsigned_short;
      Dev_Info : unsigned_long;
   end record;

   type AST_Handler is access procedure (Param : Address);
   pragma Convention (C, AST_Handler);
   No_AST_Handler : constant AST_Handler := null;

   CMB_M_READONLY  : constant := 16#00000001#;
   CMB_M_WRITEONLY : constant := 16#00000002#;
   AGN_M_READONLY  : constant := 16#00000001#;
   AGN_M_WRITEONLY : constant := 16#00000002#;

   IO_WRITEVBLK : constant := 48;  --  WRITE VIRTUAL BLOCK
   IO_READVBLK  : constant := 49;  --  READ VIRTUAL BLOCK

   ----------------
   -- Sys_Assign --
   ----------------
   --
   --  Assign I/O Channel
   --
   --  Status = returned status
   --  Devnam = address  of  device  name  or  logical  name   string
   --               descriptor
   --  Chan   = address of word to receive channel number assigned
   --  Acmode = access mode associated with channel
   --  Mbxnam = address of mailbox logical name string descriptor, if
   --               mailbox associated with device
   --  Flags  = optional channel flags longword for specifying options
   --           for the $ASSIGN operation
   --

   procedure Sys_Assign
     (Status : out Cond_Value_Type;
      Devnam : String;
      Chan   : out unsigned_short;
      Acmode : unsigned_short := 0;
      Mbxnam : String := String'Null_Parameter;
      Flags  : unsigned_long := 0);
   pragma Interface (External, Sys_Assign);
   pragma Import_Valued_Procedure
     (Sys_Assign, "SYS$ASSIGN",
      (Cond_Value_Type, String,         unsigned_short,
       unsigned_short,  String,         unsigned_long),
      (Value,           Descriptor (s), Reference,
       Value,           Descriptor (s), Value),
      Flags);

   ----------------
   -- Sys_Cantim --
   ----------------
   --
   --  Cancel Timer
   --
   --  Status  = returned status
   --  Reqidt  = ID of timer to be cancelled
   --  Acmode  = Access mode
   --
   procedure Sys_Cantim
     (Status : out Cond_Value_Type;
      Reqidt : Address;
      Acmode : unsigned);
   pragma Interface (External, Sys_Cantim);
   pragma Import_Valued_Procedure
     (Sys_Cantim, "SYS$CANTIM",
      (Cond_Value_Type, Address, unsigned),
      (Value,           Value,   Value));

   ----------------
   -- Sys_Crembx --
   ----------------
   --
   --  Create mailbox
   --
   --     Status  = returned status
   --     Prmflg  = permanent flag
   --     Chan    = channel
   --     Maxmsg  = maximum message
   --     Bufquo  = buufer quote
   --     Promsk  = protection mast
   --     Acmode  = access mode
   --     Lognam  = logical name
   --     Flags   = flags
   --
   procedure Sys_Crembx
     (Status : out Cond_Value_Type;
      Prmflg : unsigned_char;
      Chan   : out unsigned_short;
      Maxmsg : unsigned_long := 0;
      Bufquo : unsigned_long := 0;
      Promsk : unsigned_short := 0;
      Acmode : unsigned_short := 0;
      Lognam : String;
      Flags  : unsigned_long := 0);
   pragma Interface (External, Sys_Crembx);
   pragma Import_Valued_Procedure
     (Sys_Crembx, "SYS$CREMBX",
      (Cond_Value_Type, unsigned_char,  unsigned_short,
       unsigned_long,   unsigned_long,  unsigned_short,
       unsigned_short,  String,         unsigned_long),
      (Value,           Value,          Reference,
       Value,           Value,          Value,
       Value,           Descriptor (s), Value));

   -------------
   -- Sys_QIO --
   -------------
   --
   --    Queue I/O
   --
   --     Status = Returned status of call
   --     EFN    = event flag to be set when I/O completes
   --     Chan   = channel
   --     Func   = function
   --     Iosb   = I/O status block
   --     Astadr = system trap to be generated when I/O completes
   --     Astprm = AST parameter
   --     P1-6   = optional parameters

   procedure Sys_QIO
     (Status : out Cond_Value_Type;
      EFN    : unsigned_long := 0;
      Chan   : unsigned_short;
      Func   : unsigned_long := 0;
      Iosb   : out IO_Status_Block_Type;
      Astadr : AST_Handler := No_AST_Handler;
      Astprm : Address := Null_Address;
      P1     : unsigned_long := 0;
      P2     : unsigned_long := 0;
      P3     : unsigned_long := 0;
      P4     : unsigned_long := 0;
      P5     : unsigned_long := 0;
      P6     : unsigned_long := 0);

   procedure Sys_QIO
     (Status : out Cond_Value_Type;
      EFN    : unsigned_long := 0;
      Chan   : unsigned_short;
      Func   : unsigned_long := 0;
      Iosb   : Address := Null_Address;
      Astadr : AST_Handler := No_AST_Handler;
      Astprm : Address := Null_Address;
      P1     : unsigned_long := 0;
      P2     : unsigned_long := 0;
      P3     : unsigned_long := 0;
      P4     : unsigned_long := 0;
      P5     : unsigned_long := 0;
      P6     : unsigned_long := 0);

   pragma Interface (External, Sys_QIO);
   pragma Import_Valued_Procedure
     (Sys_QIO, "SYS$QIO",
      (Cond_Value_Type,      unsigned_long, unsigned_short, unsigned_long,
       IO_Status_Block_Type, AST_Handler,   Address,
       unsigned_long,        unsigned_long, unsigned_long,
       unsigned_long,        unsigned_long, unsigned_long),
      (Value,                Value,         Value,          Value,
       Reference,            Value,         Value,
       Value,                Value,         Value,
       Value,                Value,         Value));

   pragma Import_Valued_Procedure
     (Sys_QIO, "SYS$QIO",
      (Cond_Value_Type, unsigned_long, unsigned_short, unsigned_long,
       Address,         AST_Handler,   Address,
       unsigned_long,   unsigned_long, unsigned_long,
       unsigned_long,   unsigned_long, unsigned_long),
      (Value,           Value,         Value,          Value,
       Value,           Value,         Value,
       Value,           Value,         Value,
       Value,           Value,         Value));

   ----------------
   -- Sys_Setimr --
   ----------------
   --
   --    Set Timer
   --
   --     Status = Returned status of call
   --     EFN    = event flag to be set when timer expires
   --     Tim    = expiration time
   --     AST    = system trap to be generated when timer expires
   --     Redidt = returned ID of timer (e.g. to cancel timer)
   --     Flags  = flags
   --
   procedure Sys_Setimr
     (Status : out Cond_Value_Type;
      EFN    : unsigned_long;
      Tim    : Long_Integer;
      AST    : AST_Handler;
      Reqidt : Address;
      Flags  : unsigned_long);
   pragma Interface (External, Sys_Setimr);
   pragma Import_Valued_Procedure
     (Sys_Setimr, "SYS$SETIMR",
      (Cond_Value_Type, unsigned_long,     Long_Integer,
       AST_Handler,     Address,           unsigned_long),
      (Value,           Value,             Reference,
       Value,           Value,             Value));

   Interrupt_ID_0   : constant  := 0;
   Interrupt_ID_1   : constant  := 1;
   Interrupt_ID_2   : constant  := 2;
   Interrupt_ID_3   : constant  := 3;
   Interrupt_ID_4   : constant  := 4;
   Interrupt_ID_5   : constant  := 5;
   Interrupt_ID_6   : constant  := 6;
   Interrupt_ID_7   : constant  := 7;
   Interrupt_ID_8   : constant  := 8;
   Interrupt_ID_9   : constant  := 9;
   Interrupt_ID_10  : constant  := 10;
   Interrupt_ID_11  : constant  := 11;
   Interrupt_ID_12  : constant  := 12;
   Interrupt_ID_13  : constant  := 13;
   Interrupt_ID_14  : constant  := 14;
   Interrupt_ID_15  : constant  := 15;
   Interrupt_ID_16  : constant  := 16;
   Interrupt_ID_17  : constant  := 17;
   Interrupt_ID_18  : constant  := 18;
   Interrupt_ID_19  : constant  := 19;
   Interrupt_ID_20  : constant  := 20;
   Interrupt_ID_21  : constant  := 21;
   Interrupt_ID_22  : constant  := 22;
   Interrupt_ID_23  : constant  := 23;
   Interrupt_ID_24  : constant  := 24;
   Interrupt_ID_25  : constant  := 25;
   Interrupt_ID_26  : constant  := 26;
   Interrupt_ID_27  : constant  := 27;
   Interrupt_ID_28  : constant  := 28;
   Interrupt_ID_29  : constant  := 29;
   Interrupt_ID_30  : constant  := 30;
   Interrupt_ID_31  : constant  := 31;

   -----------
   -- Errno --
   -----------

   function errno return int;
   pragma Import (C, errno, "__get_errno");

   EINTR  : constant := 4;   --  Interrupted system call
   EAGAIN : constant := 11;  --  No more processes
   ENOMEM : constant := 12;  --  Not enough core

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_FIFO  : constant := 1;
   SCHED_RR    : constant := 2;
   SCHED_OTHER : constant := 3;
   SCHED_BG    : constant := 4;
   SCHED_LFI   : constant := 5;
   SCHED_LRR   : constant := 6;

   -------------
   -- Process --
   -------------

   type pid_t is private;

   function kill (pid : pid_t; sig : Signal) return int;
   pragma Import (C, kill);

   function getpid return pid_t;
   pragma Import (C, getpid);

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;
   pragma Convention (C, Thread_Body);

   function Thread_Body_Access is new
     Ada.Unchecked_Conversion (System.Aux_DEC.Short_Address, Thread_Body);

   type pthread_t           is private;
   subtype Thread_Id        is pthread_t;

   type pthread_mutex_t     is limited private;
   type pthread_cond_t      is limited private;
   type pthread_attr_t      is limited private;
   type pthread_mutexattr_t is limited private;
   type pthread_condattr_t  is limited private;
   type pthread_key_t       is private;

   PTHREAD_CREATE_JOINABLE     : constant := 0;
   PTHREAD_CREATE_DETACHED     : constant := 1;

   PTHREAD_CANCEL_DISABLE      : constant := 0;
   PTHREAD_CANCEL_ENABLE       : constant := 1;

   PTHREAD_CANCEL_DEFERRED     : constant := 0;
   PTHREAD_CANCEL_ASYNCHRONOUS : constant := 1;

   --  Don't use ERRORCHECK mutexes, they don't work when a thread is not
   --  the owner.  AST's, at least, unlock others threads mutexes. Even
   --  if the error is ignored, they don't work.
   PTHREAD_MUTEX_NORMAL_NP     : constant := 0;
   PTHREAD_MUTEX_RECURSIVE_NP  : constant := 1;
   PTHREAD_MUTEX_ERRORCHECK_NP : constant := 2;

   PTHREAD_INHERIT_SCHED       : constant := 0;
   PTHREAD_EXPLICIT_SCHED      : constant := 1;

   function pthread_cancel (thread : pthread_t) return int;
   pragma Import (C, pthread_cancel, "PTHREAD_CANCEL");

   procedure pthread_testcancel;
   pragma Import (C, pthread_testcancel, "PTHREAD_TESTCANCEL");

   function pthread_setcancelstate
     (newstate : int; oldstate : access int) return int;
   pragma Import (C, pthread_setcancelstate, "PTHREAD_SETCANCELSTATE");

   function pthread_setcanceltype
     (newtype : int; oldtype : access int) return int;
   pragma Import (C, pthread_setcanceltype, "PTHREAD_SETCANCELTYPE");

   -------------------------
   -- POSIX.1c  Section 3 --
   -------------------------

   function pthread_lock_global_np return int;
   pragma Import (C, pthread_lock_global_np, "PTHREAD_LOCK_GLOBAL_NP");

   function pthread_unlock_global_np return int;
   pragma Import (C, pthread_unlock_global_np, "PTHREAD_UNLOCK_GLOBAL_NP");

   --------------------------
   -- POSIX.1c  Section 11 --
   --------------------------

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutexattr_init, "PTHREAD_MUTEXATTR_INIT");

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutexattr_destroy, "PTHREAD_MUTEXATTR_DESTROY");

   function pthread_mutexattr_settype_np
     (attr      : access pthread_mutexattr_t;
      mutextype : int) return int;
   pragma Import (C, pthread_mutexattr_settype_np,
                     "PTHREAD_MUTEXATTR_SETTYPE_NP");

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutex_init, "PTHREAD_MUTEX_INIT");

   function pthread_mutex_destroy (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_destroy, "PTHREAD_MUTEX_DESTROY");

   function pthread_mutex_lock (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_lock, "PTHREAD_MUTEX_LOCK");

   function pthread_mutex_unlock (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_unlock, "PTHREAD_MUTEX_UNLOCK");

   function pthread_mutex_setname_np
     (attr : access pthread_mutex_t;
      name : System.Address;
      mbz  : System.Address) return int;
   pragma Import (C, pthread_mutex_setname_np, "PTHREAD_MUTEX_SETNAME_NP");

   function pthread_condattr_init
     (attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_condattr_init, "PTHREAD_CONDATTR_INIT");

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_condattr_destroy, "PTHREAD_CONDATTR_DESTROY");

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_cond_init, "PTHREAD_COND_INIT");

   function pthread_cond_destroy (cond : access pthread_cond_t) return int;
   pragma Import (C, pthread_cond_destroy, "PTHREAD_COND_DESTROY");

   function pthread_cond_signal (cond : access pthread_cond_t) return int;
   pragma Import (C, pthread_cond_signal, "PTHREAD_COND_SIGNAL");

   function pthread_cond_signal_int_np
     (cond : access pthread_cond_t) return int;
   pragma Import (C, pthread_cond_signal_int_np,
                  "PTHREAD_COND_SIGNAL_INT_NP");

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_cond_wait, "PTHREAD_COND_WAIT");

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------

   function pthread_mutexattr_setprotocol
     (attr : access pthread_mutexattr_t; protocol : int) return int;
   pragma Import (C, pthread_mutexattr_setprotocol,
                     "PTHREAD_MUTEXATTR_SETPROTOCOL");

   type struct_sched_param is record
      sched_priority : int;  --  scheduling priority
   end record;
   for struct_sched_param'Size use 8*4;
   pragma Convention (C, struct_sched_param);

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int;
   pragma Import (C, pthread_setschedparam, "PTHREAD_SETSCHEDPARAM");

   function pthread_attr_setscope
     (attr            : access pthread_attr_t;
      contentionscope : int) return int;
   pragma Import (C, pthread_attr_setscope, "PTHREAD_ATTR_SETSCOPE");

   function pthread_attr_setinheritsched
     (attr            : access pthread_attr_t;
      inheritsched : int) return int;
   pragma Import (C, pthread_attr_setinheritsched,
                     "PTHREAD_ATTR_SETINHERITSCHED");

   function pthread_attr_setschedpolicy
     (attr : access pthread_attr_t; policy : int) return int;
   pragma Import (C, pthread_attr_setschedpolicy,
                     "PTHREAD_ATTR_SETSCHEDPOLICY");

   function pthread_attr_setschedparam
     (attr        : access pthread_attr_t;
      sched_param : int) return int;
   pragma Import (C, pthread_attr_setschedparam, "PTHREAD_ATTR_SETSCHEDPARAM");

   function pthread_attr_setname_np
     (attr : access pthread_attr_t;
      name : System.Address;
      mbz  : System.Address) return int;
   pragma Import (C, pthread_attr_setname_np, "PTHREAD_ATTR_SETNAME_NP");

   function sched_yield return int;

   --------------------------
   -- P1003.1c  Section 16 --
   --------------------------

   function pthread_attr_init (attributes : access pthread_attr_t) return int;
   pragma Import (C, pthread_attr_init, "PTHREAD_ATTR_INIT");

   function pthread_attr_destroy
     (attributes : access pthread_attr_t) return int;
   pragma Import (C, pthread_attr_destroy, "PTHREAD_ATTR_DESTROY");

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int) return int;
   pragma Import (C, pthread_attr_setdetachstate,
                     "PTHREAD_ATTR_SETDETACHSTATE");

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t) return int;
   pragma Import (C, pthread_attr_setstacksize, "PTHREAD_ATTR_SETSTACKSIZE");

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address) return int;
   pragma Import (C, pthread_create, "PTHREAD_CREATE");

   procedure pthread_exit (status : System.Address);
   pragma Import (C, pthread_exit, "PTHREAD_EXIT");

   function pthread_self return pthread_t;
   pragma Import (C, pthread_self, "PTHREAD_SELF");

   --------------------------
   -- POSIX.1c  Section 17 --
   --------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address) return  int;
   pragma Import (C, pthread_setspecific, "PTHREAD_SETSPECIFIC");

   function pthread_getspecific (key : pthread_key_t) return System.Address;
   pragma Import (C, pthread_getspecific, "PTHREAD_GETSPECIFIC");

   type destructor_pointer is access procedure (arg : System.Address);
   pragma Convention (C, destructor_pointer);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int;
   pragma Import (C, pthread_key_create, "PTHREAD_KEY_CREATE");

private

   type pid_t is new int;

   type pthreadLongAddr_p is mod 2 ** Long_Integer'Size;

   type pthreadLongAddr_t is mod 2 ** Long_Integer'Size;
   type pthreadLongAddr_t_ptr is mod 2 ** Long_Integer'Size;

   type pthreadLongString_t is mod 2 ** Long_Integer'Size;

   type pthreadLongUint_t is mod 2 ** Long_Integer'Size;
   type pthreadLongUint_array is array (Natural range <>)
     of pthreadLongUint_t;

   type pthread_t is mod 2 ** Long_Integer'Size;

   type pthread_cond_t is record
      state    : unsigned;
      valid    : unsigned;
      name     : pthreadLongString_t;
      arg      : unsigned;
      sequence : unsigned;
      block    : pthreadLongAddr_t_ptr;
   end record;
   for pthread_cond_t'Size use 8*32;
   pragma Convention (C, pthread_cond_t);

   type pthread_attr_t is record
      valid    : long;
      name     : pthreadLongString_t;
      arg      : pthreadLongUint_t;
      reserved : pthreadLongUint_array (0 .. 18);
   end record;
   for pthread_attr_t'Size use 8*176;
   pragma Convention (C, pthread_attr_t);

   type pthread_mutex_t is record
      lock     : unsigned;
      valid    : unsigned;
      name     : pthreadLongString_t;
      arg      : unsigned;
      sequence : unsigned;
      block    : pthreadLongAddr_p;
      owner    : unsigned;
      depth    : unsigned;
   end record;
   for pthread_mutex_t'Size use 8*40;
   pragma Convention (C, pthread_mutex_t);

   type pthread_mutexattr_t is record
      valid    : long;
      reserved : pthreadLongUint_array (0 .. 14);
   end record;
   for pthread_mutexattr_t'Size use 8*128;
   pragma Convention (C, pthread_mutexattr_t);

   type pthread_condattr_t is record
      valid    : long;
      reserved : pthreadLongUint_array (0 .. 12);
   end record;
   for pthread_condattr_t'Size use 8*112;
   pragma Convention (C, pthread_condattr_t);

   type pthread_key_t is new unsigned;

   pragma Inline (pthread_self);

end System.OS_Interface;
