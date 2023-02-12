------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I M P U N I T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2000-2023, Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Errout;      use Errout;
with Sinfo;       use Sinfo;
with Sinfo.Nodes; use Sinfo.Nodes;
with Fname.UF;    use Fname.UF;
with Lib;         use Lib;
with Namet;       use Namet;
with Opt;         use Opt;
with Uname;       use Uname;

--  Note: this package body is used by GNAT Studio and GNATBench to supply a
--  list of entries for help on available library routines.

package body Impunit is

   subtype File_Name_8 is String (1 .. 8);

   type File_Name_Record is record
      Fname : File_Name_8;
      --  8 character name of unit

      RMdef : Boolean;
      --  True if unit is RM defined. False for any unit that is implementation
      --  defined (and thus not with'able in No_Implementation_Units mode).
   end record;

   type File_List is array (Nat range <>) of File_Name_Record;

   T : constant Boolean := True;
   F : constant Boolean := False;
   --  Short hand for RM_Defined values in lists below

   ------------------
   -- Ada 95 Units --
   ------------------

   --  The following is a giant string list containing the names of all non-
   --  implementation internal files, i.e. the complete list of files for
   --  internal units which a program may legitimately WITH when operating in
   --  either Ada 95 or Ada 2005 mode.

   --  Note that this list should match the list of units documented in the
   --  "GNAT Library" section of the GNAT Reference Manual. A unit listed here
   --  must either be documented in that section or described in the Ada RM.

   Non_Imp_File_Names_95 : constant File_List := (

   ------------------------------------------------------
   -- Ada Hierarchy Units from Ada-95 Reference Manual --
   ------------------------------------------------------

    ("a-astaco", T),  -- Ada.Asynchronous_Task_Control
    ("a-calend", T),  -- Ada.Calendar
    ("a-chahan", T),  -- Ada.Characters.Handling
    ("a-charac", T),  -- Ada.Characters
    ("a-chlat1", T),  -- Ada.Characters.Latin_1
    ("a-comlin", T),  -- Ada.Command_Line
    ("a-decima", T),  -- Ada.Decimal
    ("a-direio", T),  -- Ada.Direct_IO
    ("a-dynpri", T),  -- Ada.Dynamic_Priorities
    ("a-except", T),  -- Ada.Exceptions
    ("a-finali", T),  -- Ada.Finalization
    ("a-flteio", T),  -- Ada.Float_Text_IO
    ("a-fwteio", T),  -- Ada.Float_Wide_Text_IO
    ("a-inteio", T),  -- Ada.Integer_Text_IO
    ("a-interr", T),  -- Ada.Interrupts
    ("a-intnam", T),  -- Ada.Interrupts.Names
    ("a-ioexce", T),  -- Ada.IO_Exceptions
    ("a-iwteio", T),  -- Ada.Integer_Wide_Text_IO
    ("a-ncelfu", T),  -- Ada.Numerics.Complex_Elementary_Functions
    ("a-ngcefu", T),  -- Ada.Numerics.Generic_Complex_Elementary_Functions
    ("a-ngcoty", T),  -- Ada.Numerics.Generic_Complex_Types
    ("a-ngelfu", T),  -- Ada.Numerics.Generic_Elementary_Functions
    ("a-nucoty", T),  -- Ada.Numerics.Complex_Types
    ("a-nudira", T),  -- Ada.Numerics.Discrete_Random
    ("a-nuelfu", T),  -- Ada.Numerics.Elementary_Functions
    ("a-nuflra", T),  -- Ada.Numerics.Float_Random
    ("a-numeri", T),  -- Ada.Numerics
    ("a-reatim", T),  -- Ada.Real_Time
    ("a-sequio", T),  -- Ada.Sequential_IO
    ("a-stmaco", T),  -- Ada.Strings.Maps.Constants
    ("a-storio", T),  -- Ada.Storage_IO
    ("a-strbou", T),  -- Ada.Strings.Bounded
    ("a-stream", T),  -- Ada.Streams
    ("a-strfix", T),  -- Ada.Strings.Fixed
    ("a-string", T),  -- Ada.Strings
    ("a-strmap", T),  -- Ada.Strings.Maps
    ("a-strunb", T),  -- Ada.Strings.Unbounded
    ("a-ststio", T),  -- Ada.Streams.Stream_IO
    ("a-stwibo", T),  -- Ada.Strings.Wide_Bounded
    ("a-stwifi", T),  -- Ada.Strings.Wide_Fixed
    ("a-stwima", T),  -- Ada.Strings.Wide_Maps
    ("a-stwiun", T),  -- Ada.Strings.Wide_Unbounded
    ("a-swmwco", T),  -- Ada.Strings.Wide_Maps.Wide_Constants
    ("a-sytaco", T),  -- Ada.Synchronous_Task_Control
    ("a-tags  ", T),  -- Ada.Tags
    ("a-tasatt", T),  -- Ada.Task_Attributes
    ("a-taside", T),  -- Ada.Task_Identification
    ("a-teioed", T),  -- Ada.Text_IO.Editing
    ("a-textio", T),  -- Ada.Text_IO
    ("a-ticoio", T),  -- Ada.Text_IO.Complex_IO
    ("a-titest", T),  -- Ada.Text_IO.Text_Streams
    ("a-unccon", T),  -- Ada.Unchecked_Conversion
    ("a-uncdea", T),  -- Ada.Unchecked_Deallocation
    ("a-witeio", T),  -- Ada.Wide_Text_IO
    ("a-wtcoio", T),  -- Ada.Wide_Text_IO.Complex_IO
    ("a-wtedit", T),  -- Ada.Wide_Text_IO.Editing
    ("a-wttest", T),  -- Ada.Wide_Text_IO.Text_Streams

   -------------------------------------------------
   -- RM Required Additions to Ada for GNAT Types --
   -------------------------------------------------

   --  Note: Long versions are considered RM defined, but not the Long Long,
   --  Short, or Short_Short versions.

    ("a-lfteio", T),  -- Ada.Long_Float_Text_IO
    ("a-lfwtio", T),  -- Ada.Long_Float_Wide_Text_IO
    ("a-liteio", T),  -- Ada.Long_Integer_Text_IO
    ("a-liwtio", T),  -- Ada.Long_Integer_Wide_Text_IO
    ("a-llftio", T),  -- Ada.Long_Long_Float_Text_IO
    ("a-llfwti", T),  -- Ada.Long_Long_Float_Wide_Text_IO
    ("a-llitio", T),  -- Ada.Long_Long_Integer_Text_IO
    ("a-lliwti", F),  -- Ada.Long_Long_Integer_Wide_Text_IO
    ("a-llltio", T),  -- Ada.Long_Long_Long_Integer_Text_IO
    ("a-lllwti", F),  -- Ada.Long_Long_Long_Integer_Wide_Text_IO
    ("a-nlcefu", F),  -- Ada.Long_Complex_Elementary_Functions
    ("a-nlcoty", T),  -- Ada.Numerics.Long_Complex_Types
    ("a-nlelfu", T),  -- Ada.Numerics.Long_Elementary_Functions
    ("a-nllcef", F),  -- Ada.Long_Long_Complex_Elementary_Functions
    ("a-nllefu", F),  -- Ada.Numerics.Long_Long_Elementary_Functions
    ("a-nllcty", F),  -- Ada.Numerics.Long_Long_Complex_Types
    ("a-nscefu", F),  -- Ada.Short_Complex_Elementary_Functions
    ("a-nscoty", F),  -- Ada.Numerics.Short_Complex_Types
    ("a-nselfu", F),  -- Ada.Numerics.Short_Elementary_Functions
    ("a-sfteio", F),  -- Ada.Short_Float_Text_IO
    ("a-sfwtio", F),  -- Ada.Short_Float_Wide_Text_IO
    ("a-siteio", F),  -- Ada.Short_Integer_Text_IO
    ("a-siwtio", F),  -- Ada.Short_Integer_Wide_Text_IO
    ("a-ssitio", F),  -- Ada.Short_Short_Integer_Text_IO
    ("a-ssiwti", F),  -- Ada.Short_Short_Integer_Wide_Text_IO

   -----------------------------------
   -- GNAT Defined Additions to Ada --
   -----------------------------------

    ("a-calcon", F),  -- Ada.Calendar.Conversions
    ("a-chlat9", F),  -- Ada.Characters.Latin_9
    ("a-clrefi", F),  -- Ada.Command_Line.Response_File
    ("a-colien", F),  -- Ada.Command_Line.Environment
    ("a-colire", F),  -- Ada.Command_Line.Remove
    ("a-cwila1", F),  -- Ada.Characters.Wide_Latin_1
    ("a-cwila9", F),  -- Ada.Characters.Wide_Latin_9
    ("a-diocst", F),  -- Ada.Direct_IO.C_Streams
    ("a-einuoc", F),  -- Ada.Exceptions.Is_Null_Occurrence
    ("a-elchha", F),  -- Ada.Exceptions.Last_Chance_Handler
    ("a-exctra", F),  -- Ada.Exceptions.Traceback
    ("a-siocst", F),  -- Ada.Sequential_IO.C_Streams
    ("a-ssicst", F),  -- Ada.Streams.Stream_IO.C_Streams
    ("a-suteio", F),  -- Ada.Strings.Unbounded.Text_IO
    ("a-swuwti", F),  -- Ada.Strings.Wide_Unbounded.Wide_Text_IO
    ("a-tasini", F),  -- Ada.Task_Initialization
    ("a-tiocst", F),  -- Ada.Text_IO.C_Streams
    ("a-wtcstr", F),  -- Ada.Wide_Text_IO.C_Streams

      --  Note: strictly the next two should be Ada 2005 units, but it seems
      --  harmless (and useful) to make then available in Ada 95 mode, since
      --  they only deal with Wide_Character, not Wide_Wide_Character.

    ("a-wichun", F),  -- Ada.Wide_Characters.Unicode
    ("a-widcha", F),  -- Ada.Wide_Characters

      --  Note: strictly the following should be Ada 2012 units, but it seems
      --  harmless (and useful) to make then available in Ada 95 mode, since
      --  they do not deal with Wide_Wide_Character.

    ("a-wichha", F),  -- Ada.Wide_Characters.Handling
    ("a-stuten", F),  -- Ada.Strings.UTF_Encoding
    ("a-suenco", F),  -- Ada.Strings.UTF_Encoding.Conversions
    ("a-suenst", F),  -- Ada.Strings.UTF_Encoding.Strings
    ("a-suewst", F),  -- Ada.Strings.UTF_Encoding.Wide_Strings

   ---------------------------
   -- GNAT Special IO Units --
   ---------------------------

   --  As further explained elsewhere (see Sem_Ch10), the internal packages of
   --  Text_IO and Wide_Text_IO are actually implemented as separate children,
   --  but this fact is intended to be hidden from the user completely. Any
   --  attempt to WITH one of these units will be diagnosed as an error later
   --  on, but for now we do not consider these internal implementation units
   --  (if we did, then we would get a junk warning which would be confusing
   --  and unnecessary, given that we generate a clear error message).

    ("a-tideio", F),  -- Ada.Text_IO.Decimal_IO
    ("a-tienio", F),  -- Ada.Text_IO.Enumeration_IO
    ("a-tifiio", F),  -- Ada.Text_IO.Fixed_IO
    ("a-tiflio", F),  -- Ada.Text_IO.Float_IO
    ("a-tiinio", F),  -- Ada.Text_IO.Integer_IO
    ("a-timoio", F),  -- Ada.Text_IO.Modular_IO
    ("a-wtdeio", F),  -- Ada.Wide_Text_IO.Decimal_IO
    ("a-wtenio", F),  -- Ada.Wide_Text_IO.Enumeration_IO
    ("a-wtfiio", F),  -- Ada.Wide_Text_IO.Fixed_IO
    ("a-wtflio", F),  -- Ada.Wide_Text_IO.Float_IO
    ("a-wtinio", F),  -- Ada.Wide_Text_IO.Integer_IO
    ("a-wtmoio", F),  -- Ada.Wide_Text_IO.Modular_IO

   ------------------------
   -- GNAT Library Units --
   ------------------------

    ("g-altive", F),  -- GNAT.Altivec
    ("g-altcon", F),  -- GNAT.Altivec.Conversions
    ("g-alveop", F),  -- GNAT.Altivec.Vector_Operations
    ("g-alvety", F),  -- GNAT.Altivec.Vector_Types
    ("g-alvevi", F),  -- GNAT.Altivec.Vector_Views
    ("g-arrspl", F),  -- GNAT.Array_Split
    ("g-awk   ", F),  -- GNAT.AWK
    ("g-binenv", F),  -- GNAT.Bind_Environment
    ("g-binsea", F),  -- GNAT.Binary_Search
    ("g-boubuf", F),  -- GNAT.Bounded_Buffers
    ("g-boumai", F),  -- GNAT.Bounded_Mailboxes
    ("g-brapre", F),  -- GNAT.Branch_Prediction
    ("g-bubsor", F),  -- GNAT.Bubble_Sort
    ("g-busora", F),  -- GNAT.Bubble_Sort_A
    ("g-busorg", F),  -- GNAT.Bubble_Sort_G
    ("g-byorma", F),  -- GNAT.Byte_Order_Mark
    ("g-bytswa", F),  -- GNAT.Byte_Swapping
    ("g-calend", F),  -- GNAT.Calendar
    ("g-catiio", F),  -- GNAT.Calendar.Time_IO
    ("g-casuti", F),  -- GNAT.Case_Util
    ("g-cgi   ", F),  -- GNAT.CGI
    ("g-cgicoo", F),  -- GNAT.CGI.Cookie
    ("g-cgideb", F),  -- GNAT.CGI.Debug
    ("g-comlin", F),  -- GNAT.Command_Line
    ("g-comver", F),  -- GNAT.Compiler_Version
    ("g-cppexc", F),  -- GNAT.CPP_Exceptions
    ("g-crc32 ", F),  -- GNAT.CRC32
    ("g-ctrl_c", F),  -- GNAT.Ctrl_C
    ("g-curexc", F),  -- GNAT.Current_Exception
    ("g-debpoo", F),  -- GNAT.Debug_Pools
    ("g-debuti", F),  -- GNAT.Debug_Utilities
    ("g-decstr", F),  -- GNAT.Decode_String
    ("g-deutst", F),  -- GNAT.Decode_UTF8_String
    ("g-dirope", F),  -- GNAT.Directory_Operations
    ("g-diopit", F),  -- GNAT.Directory_Operations.Iteration
    ("g-dynhta", F),  -- GNAT.Dynamic_HTables
    ("g-dyntab", F),  -- GNAT.Dynamic_Tables
    ("g-encstr", F),  -- GNAT.Encode_String
    ("g-enutst", F),  -- GNAT.Encode_UTF8_String
    ("g-excact", F),  -- GNAT.Exception_Actions
    ("g-except", F),  -- GNAT.Exceptions
    ("g-exctra", F),  -- GNAT.Exception_Traces
    ("g-expect", F),  -- GNAT.Expect
    ("g-exptty", F),  -- GNAT.Expect.TTY
    ("g-flocon", F),  -- GNAT.Float_Control
    ("g-forstr", F),  -- GNAT.Formatted_String
    ("g-gfmafu", F),  -- GNAT.Generic_Fast_Math_Functions
    ("g-graphs", F),  -- GNAT.Graphs
    ("g-heasor", F),  -- GNAT.Heap_Sort
    ("g-hesora", F),  -- GNAT.Heap_Sort_A
    ("g-hesorg", F),  -- GNAT.Heap_Sort_G
    ("g-htable", F),  -- GNAT.Htable
    ("g-io    ", F),  -- GNAT.IO
    ("g-io_aux", F),  -- GNAT.IO_Aux
    ("g-lists ", F),  -- GNAT.Lists
    ("g-locfil", F),  -- GNAT.Lock_Files
    ("g-mbdira", F),  -- GNAT.MBBS_Discrete_Random
    ("g-mbflra", F),  -- GNAT.MBBS_Float_Random
    ("g-md5   ", F),  -- GNAT.MD5
    ("g-memdum", F),  -- GNAT.Memory_Dump
    ("g-moreex", F),  -- GNAT.Most_Recent_Exception
    ("g-os_lib", F),  -- GNAT.Os_Lib
    ("g-pehage", F),  -- GNAT.Perfect_Hash_Generators
    ("g-rannum", F),  -- GNAT.Random_Numbers
    ("g-regexp", F),  -- GNAT.Regexp
    ("g-regist", F),  -- GNAT.Registry
    ("g-regpat", F),  -- GNAT.Regpat
    ("g-rewdat", F),  -- GNAT.Rewrite_Data
    ("g-semaph", F),  -- GNAT.Semaphores
    ("g-sercom", F),  -- GNAT.Serial_Communications
    ("g-sestin", F),  -- GNAT.Secondary_Stack_Info
    ("g-sets  ", F),  -- GNAT.Sets
    ("g-sha1  ", F),  -- GNAT.SHA1
    ("g-sha224", F),  -- GNAT.SHA224
    ("g-sha256", F),  -- GNAT.SHA256
    ("g-sha384", F),  -- GNAT.SHA384
    ("g-sha512", F),  -- GNAT.SHA512
    ("g-signal", F),  -- GNAT.Signals
    ("g-socket", F),  -- GNAT.Sockets
    ("g-socpol", F),  -- GNAT.Sockets.Poll
    ("g-souinf", F),  -- GNAT.Source_Info
    ("g-speche", F),  -- GNAT.Spell_Checker
    ("g-spchge", F),  -- GNAT.Spell_Checker_Generic
    ("g-spitbo", F),  -- GNAT.Spitbol
    ("g-spipat", F),  -- GNAT.Spitbol.Patterns
    ("g-sptabo", F),  -- GNAT.Spitbol.Table_Boolean
    ("g-sptain", F),  -- GNAT.Spitbol.Table_Integer
    ("g-sptavs", F),  -- GNAT.Spitbol.Table_Vstring
    ("g-strhas", F),  -- GNAT.String_Hash
    ("g-string", F),  -- GNAT.Strings
    ("g-strspl", F),  -- GNAT.String_Split
    ("g-sse   ", F),  -- GNAT.SSE
    ("g-ssvety", F),  -- GNAT.SSE.Vector_Types
    ("g-table ", F),  -- GNAT.Table
    ("g-tasloc", F),  -- GNAT.Task_Lock
    ("g-tastus", F),  -- GNAT.Task_Stack_Usage
    ("g-thread", F),  -- GNAT.Threads
    ("g-timsta", F),  -- GNAT.Time_Stamp
    ("g-traceb", F),  -- GNAT.Traceback
    ("g-trasym", F),  -- GNAT.Traceback.Symbolic
    ("g-tty   ", F),  -- GNAT.TTY
    ("g-utf_32", F),  -- GNAT.UTF_32
    ("g-u3spch", F),  -- GNAT.UTF_32_Spelling_Checker
    ("g-wispch", F),  -- GNAT.Wide_Spelling_Checker
    ("g-wistsp", F),  -- GNAT.Wide_String_Split

   -----------------------------------------------------
   -- Interface Hierarchy Units from Reference Manual --
   -----------------------------------------------------

    ("i-c     ", T),  -- Interfaces.C
    ("i-cobol ", T),  -- Interfaces.Cobol
    ("i-cpoint", T),  -- Interfaces.C.Pointers
    ("i-cstrin", T),  -- Interfaces.C.Strings
    ("i-fortra", T),  -- Interfaces.Fortran

   ------------------------------------------
   -- GNAT Defined Additions to Interfaces --
   ------------------------------------------

    ("i-cexten", F),  -- Interfaces.C.Extensions
    ("i-cil   ", F),  -- Interfaces.CIL
    ("i-cilobj", F),  -- Interfaces.CIL.Object
    ("i-cstrea", F),  -- Interfaces.C.Streams
    ("i-java  ", F),  -- Interfaces.Java
    ("i-javjni", F),  -- Interfaces.Java.JNI
    ("i-pacdec", F),  -- Interfaces.Packed_Decimal
    ("i-vxinco", F),  -- Interfaces.VxWorks.Int_Connection
    ("i-vxwoio", F),  -- Interfaces.VxWorks.IO
    ("i-vxwork", F),  -- Interfaces.VxWorks

   --------------------------------------------------
   -- System Hierarchy Units from Reference Manual --
   --------------------------------------------------

    ("s-atacco", T),  -- System.Address_To_Access_Conversions
    ("s-maccod", T),  -- System.Machine_Code
    ("s-rpc   ", T),  -- System.Rpc
    ("s-stoele", T),  -- System.Storage_Elements
    ("s-stopoo", T),  -- System.Storage_Pools

   --------------------------------------
   -- GNAT Defined Additions to System --
   --------------------------------------

    ("s-addima", F),  -- System.Address_Image
    ("s-atocou", F),  -- System.Atomic_Counters
    ("s-assert", F),  -- System.Assertions
    ("s-dfmkio", F),  -- System.Dim.Float_Mks_IO
    ("s-dfmopr", F),  -- System.Dim.Float_Mks.Other_Prefixes
    ("s-dgmgop", F),  -- System.Dim.Generic_Mks.Generic_Other_Prefixes
    ("s-dlmopr", F),  -- System.Dim.Long_Mks.Other_Prefixes
    ("s-diflio", F),  -- System.Dim.Float_IO
    ("s-diflmk", F),  -- System.Dim.Float_Mks
    ("s-digemk", F),  -- System.Dim.Generic_Mks
    ("s-diinio", F),  -- System.Dim.Integer_IO
    ("s-dilomk", F),  -- System.Dim.Long_Mks
    ("s-dimkio", F),  -- System.Dim.Mks_IO
    ("s-dimmks", F),  -- System.Dim.Mks
    ("s-dlmkio", F),  -- System.Dim.Long_Mks_IO
    ("s-dmotpr", F),  -- System.Dim.Mks.Other_Prefixes
    ("s-memory", F),  -- System.Memory
    ("s-parint", F),  -- System.Partition_Interface
    ("s-pooglo", F),  -- System.Pool_Global
    ("s-pooloc", F),  -- System.Pool_Local
    ("s-restri", F),  -- System.Restrictions
    ("s-rident", F),  -- System.Rident
    ("s-ststop", F),  -- System.Strings.Stream_Ops
    ("s-tasinf", F),  -- System.Task_Info
    ("s-unstyp", F),  -- System.Unsigned_Types
    ("s-wchcnv", F),  -- System.WCh_Cnv
    ("s-wchcon", F),  -- System.WCh_Con

   --  The following are strictly speaking Ada 2012 units, but we are allowed
   --  to add children to system, so we consider them to be implementation
   --  defined additions to System in earlier versions of Ada.

    ("s-multip", T),  -- System.Multiprocessors
    ("s-mudido", T)); -- System.Multiprocessors.Dispatching_Domains

   --------------------
   -- Ada 2005 Units --
   --------------------

   --  The following units should be used only in Ada 05 mode

   Non_Imp_File_Names_05 : constant File_List := (

   --------------------------------------------------------
   -- Ada Hierarchy Units from Ada 2005 Reference Manual --
   --------------------------------------------------------

    ("a-assert", T),  -- Ada.Assertions
    ("a-calari", T),  -- Ada.Calendar.Arithmetic
    ("a-calfor", T),  -- Ada.Calendar.Formatting
    ("a-catizo", T),  -- Ada.Calendar.Time_Zones
    ("a-cdlili", T),  -- Ada.Containers.Doubly_Linked_Lists
    ("a-cgarso", T),  -- Ada.Containers.Generic_Array_Sort
    ("a-cgcaso", T),  -- Ada.Containers.Generic_Constrained_Array_Sort
    ("a-chacon", T),  -- Ada.Characters.Conversions
    ("a-cidlli", T),  -- Ada.Containers.Indefinite_Doubly_Linked_Lists
    ("a-cihama", T),  -- Ada.Containers.Indefinite_Hashed_Maps
    ("a-cihase", T),  -- Ada.Containers.Indefinite_Hashed_Sets
    ("a-ciorma", T),  -- Ada.Containers.Indefinite_Ordered_Maps
    ("a-ciorse", T),  -- Ada.Containers.Indefinite_Ordered_Sets
    ("a-cohama", T),  -- Ada.Containers.Hashed_Maps
    ("a-cohase", T),  -- Ada.Containers.Hashed_Sets
    ("a-coinve", T),  -- Ada.Containers.Indefinite_Vectors
    ("a-contai", T),  -- Ada.Containers
    ("a-convec", T),  -- Ada.Containers.Vectors
    ("a-coorma", T),  -- Ada.Containers.Ordered_Maps
    ("a-coorse", T),  -- Ada.Containers.Ordered_Sets
    ("a-coteio", T),  -- Ada.Complex_Text_IO
    ("a-direct", T),  -- Ada.Directories
    ("a-dinopr", T),  -- Ada.Dispatching.Non_Preemptive
    ("a-diroro", T),  -- Ada.Dispatching.Round_Robin
    ("a-disedf", T),  -- Ada.Dispatching.EDF
    ("a-dispat", T),  -- Ada.Dispatching
    ("a-envvar", T),  -- Ada.Environment_Variables
    ("a-etgrbu", T),  -- Ada.Execution_Time.Group_Budgets
    ("a-exetim", T),  -- Ada.Execution_Time
    ("a-extiti", T),  -- Ada.Execution_Time.Timers
    ("a-izteio", T),  -- Ada.Integer_Wide_Wide_Text_IO
    ("a-rttiev", T),  -- Ada.Real_Time.Timing_Events
    ("a-ngcoar", T),  -- Ada.Numerics.Generic_Complex_Arrays
    ("a-ngrear", T),  -- Ada.Numerics.Generic_Real_Arrays
    ("a-nucoar", T),  -- Ada.Numerics.Complex_Arrays
    ("a-nurear", T),  -- Ada.Numerics.Real_Arrays
    ("a-stboha", T),  -- Ada.Strings.Bounded.Hash
    ("a-stfiha", T),  -- Ada.Strings.Fixed.Hash
    ("a-strhas", T),  -- Ada.Strings.Hash
    ("a-stunha", T),  -- Ada.Strings.Unbounded.Hash
    ("a-stwiha", T),  -- Ada.Strings.Wide_Hash
    ("a-stzbou", T),  -- Ada.Strings.Wide_Wide_Bounded
    ("a-stzfix", T),  -- Ada.Strings.Wide_Wide_Fixed
    ("a-stzhas", T),  -- Ada.Strings.Wide_Wide_Hash
    ("a-stzmap", T),  -- Ada.Strings.Wide_Wide_Maps
    ("a-stzunb", T),  -- Ada.Strings.Wide_Wide_Unbounded
    ("a-swbwha", T),  -- Ada.Strings.Wide_Bounded.Wide_Hash
    ("a-swfwha", T),  -- Ada.Strings.Wide_Fixed.Wide_Hash
    ("a-swuwha", T),  -- Ada.Strings.Wide_Unbounded.Wide_Hash
    ("a-szbzha", T),  -- Ada.Strings.Wide_Wide_Bounded.Wide_Wide_Hash
    ("a-szfzha", T),  -- Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash
    ("a-szmzco", T),  -- Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants
    ("a-szuzha", T),  -- Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash
    ("a-taster", T),  -- Ada.Task_Termination
    ("a-tgdico", T),  -- Ada.Tags.Generic_Dispatching_Constructor
    ("a-tiboio", T),  -- Ada.Text_IO.Bounded_IO
    ("a-tiunio", T),  -- Ada.Text_IO.Unbounded_IO
    ("a-wichun", T),  -- Ada.Wide_Characters.Unicode
    ("a-wwboio", T),  -- Ada.Wide_Text_IO.Wide_Bounded_IO
    ("a-wwunio", T),  -- Ada.Wide_Text_IO.Wide_Unbounded_IO
    ("a-zchara", T),  -- Ada.Wide_Wide_Characters
    ("a-zchhan", T),  -- Ada.Wide_Wide_Characters.Handling
    ("a-ztcoio", T),  -- Ada.Wide_Wide_Text_IO.Complex_IO
    ("a-ztedit", T),  -- Ada.Wide_Wide_Text_IO.Editing
    ("a-zttest", T),  -- Ada.Wide_Wide_Text_IO.Text_Streams
    ("a-ztexio", T),  -- Ada.Wide_Wide_Text_IO
    ("a-zzboio", T),  -- Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO
    ("a-zzunio", T),  -- Ada.Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO

   ------------------------------------------------------
   -- RM Required Additions to Ada 2005 for GNAT Types --
   ------------------------------------------------------

   --  Note: Long versions are considered RM defined, but not the Long Long,
   --  Short, or Short_Short versions.

    ("a-lcteio", T),  -- Ada.Long_Complex_Text_IO
    ("a-lfztio", T),  -- Ada.Long_Float_Wide_Wide_Text_IO
    ("a-liztio", T),  -- Ada.Long_Integer_Wide_Wide_Text_IO
    ("a-llctio", T),  -- Ada.Long_Long_Complex_Text_IO
    ("a-llfzti", T),  -- Ada.Long_Long_Float_Wide_Wide_Text_IO
    ("a-llizti", T),  -- Ada.Long_Long_Integer_Wide_Wide_Text_IO
    ("a-lllzti", T),  -- Ada.Long_Long_Long_Integer_Wide_Wide_Text_IO
    ("a-nlcoar", T),  -- Ada.Numerics.Long_Complex_Arrays
    ("a-nllcar", T),  -- Ada.Numerics.Long_Long_Complex_Arrays
    ("a-nllrar", T),  -- Ada.Numerics.Long_Long_Real_Arrays
    ("a-nlrear", T),  -- Ada.Numerics.Long_Real_Arrays
    ("a-scteio", F),  -- Ada.Short_Complex_Text_IO
    ("a-sfztio", F),  -- Ada.Short_Float_Wide_Wide_Text_IO
    ("a-siztio", F),  -- Ada.Short_Integer_Wide_Wide_Text_IO
    ("a-ssizti", F),  -- Ada.Short_Short_Integer_Wide_Wide_Text_IO

   ----------------------------------------
   -- GNAT Defined Additions to Ada 2005 --
   ----------------------------------------

    ("a-cgaaso", F),  -- Ada.Containers.Generic_Anonymous_Array_Sort
    ("a-chzla1", F),  -- Ada.Characters.Wide_Wide_Latin_1
    ("a-chzla9", F),  -- Ada.Characters.Wide_Wide_Latin_9
    ("a-ciormu", F),  -- Ada.Containers.Indefinite_Ordered_Multisets
    ("a-coormu", F),  -- Ada.Containers.Ordered_Multisets
    ("a-crdlli", F),  -- Ada.Containers.Restricted_Doubly_Linked_Lists
    ("a-szuzti", F),  -- Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO
    ("a-zchuni", F),  -- Ada.Wide_Wide_Characters.Unicode
    ("a-ztcstr", F),  -- Ada.Wide_Wide_Text_IO.C_Streams

      --  Note: strictly the following should be Ada 2012 units, but it seems
      --  harmless (and useful) to make then available in Ada 2005 mode.

    ("a-cogeso", T),  -- Ada.Containers.Generic_Sort
    ("a-dhfina", T),  -- Ada.Directories.Hierarchical_File_Names
    ("a-secain", T),  -- Ada.Strings.Equal_Case_Insensitive
    ("a-shcain", T),  -- Ada.Strings.Hash_Case_Insensitive
    ("a-slcain", T),  -- Ada.Strings.Less_Case_Insensitive
    ("a-sfecin", T),  -- Ada.Strings.Fixed.Equal_Case_Insensitive
    ("a-sfhcin", T),  -- Ada.Strings.Fixed.Hash_Case_Insensitive
    ("a-sflcin", T),  -- Ada.Strings.Fixed.Less_Case_Insensitive
    ("a-sbecin", T),  -- Ada.Strings.Bounded.Equal_Case_Insensitive
    ("a-sbhcin", T),  -- Ada.Strings.Bounded.Hash_Case_Insensitive
    ("a-sblcin", T),  -- Ada.Strings.Bounded.Less_Case_Insensitive
    ("a-suecin", T),  -- Ada.Strings.Unbounded.Equal_Case_Insensitive
    ("a-suhcin", T),  -- Ada.Strings.Unbounded.Hash_Case_Insensitive
    ("a-sulcin", T),  -- Ada.Strings.Unbounded.Less_Case_Insensitive
    ("a-suezst", T),  -- Ada.Strings.UTF_Encoding.Wide_Wide_Strings

   ---------------------------
   -- GNAT Special IO Units --
   ---------------------------

   --  See Ada 95 section for further information. These packages are for the
   --  implementation of the Wide_Wide_Text_IO generic packages.

    ("a-ztdeio", F),  -- Ada.Wide_Wide_Text_IO.Decimal_IO
    ("a-ztenio", F),  -- Ada.Wide_Wide_Text_IO.Enumeration_IO
    ("a-ztfiio", F),  -- Ada.Wide_Wide_Text_IO.Fixed_IO
    ("a-ztflio", F),  -- Ada.Wide_Wide_Text_IO.Float_IO
    ("a-ztinio", F),  -- Ada.Wide_Wide_Text_IO.Integer_IO
    ("a-ztmoio", F),  -- Ada.Wide_Wide_Text_IO.Modular_IO

   ------------------------
   -- GNAT Library Units --
   ------------------------

    ("g-zspche", F),  -- GNAT.Wide_Wide_Spelling_Checker
    ("g-zstspl", F)); -- GNAT.Wide_Wide_String_Split

   --------------------
   -- Ada 2012 Units --
   --------------------

   --  The following units should be used only in Ada 2012 mode

   Non_Imp_File_Names_12 : constant File_List := (
    ("s-stposu", T),  -- System.Storage_Pools.Subpools
    ("a-cobove", T),  -- Ada.Containers.Bounded_Vectors
    ("a-cbdlli", T),  -- Ada.Containers.Bounded_Doubly_Linked_Lists
    ("a-cborse", T),  -- Ada.Containers.Bounded_Ordered_Sets
    ("a-cborma", T),  -- Ada.Containers.Bounded_Ordered_Maps
    ("a-cbhase", T),  -- Ada.Containers.Bounded_Hashed_Sets
    ("a-cbhama", T),  -- Ada.Containers.Bounded_Hashed_Maps
    ("a-coinho", T),  -- Ada.Containers.Indefinite_Holders
    ("a-comutr", T),  -- Ada.Containers.Multiway_Trees
    ("a-cimutr", T),  -- Ada.Containers.Indefinite_Multiway_Trees
    ("a-cbmutr", T),  -- Ada.Containers.Bounded_Multiway_Trees
    ("a-csquin", T),  -- Ada.Containers.Synchronized_Queue_Interfaces
    ("a-cusyqu", T),  -- Ada.Containers.Unbounded_Synchronized_Queues
    ("a-cuprqu", T),  -- Ada.Containers.Unbounded_Priority_Queues
    ("a-cbsyqu", T),  -- Ada.Containers.Bounded_Synchronized_Queues
    ("a-cbprqu", T),  -- Ada.Containers.Bounded_Priority_Queues
    ("a-extiin", T),  -- Ada.Execution_Time.Interrupts
    ("a-iteint", T),  -- Ada.Iterator_Interfaces
    ("a-locale", T),  -- Ada.Locales
    ("a-stcoed", T),  -- Ada.Synchronous_Task_Control.EDF
    ("a-synbar", T),  -- Ada.Synchronous_Barriers
    ("a-undesu", T),  -- Ada.Unchecked_Deallocate_Subpool

   ----------------------------------------
   -- GNAT Defined Additions to Ada 2012 --
   ----------------------------------------

    ("a-coboho", F),  -- Ada.Containers.Bounded_Holders
    ("a-cvgpso", F)   -- Ada.Containers.Vectors.Generic_Parallel_Sorting from
   );                 -- GNATCOLL.OMP

   --------------------
   -- Ada 2022 Units --
   --------------------

   --  The following units should be used only in Ada 2022 mode

   Non_Imp_File_Names_22 : constant File_List := (
    ("a-nubinu", T),  -- Ada.Numerics.Big_Numbers
    ("a-nbnbin", T),  -- Ada.Numerics.Big_Numbers.Big_Integers
    ("a-nbnbre", T),  -- Ada.Numerics.Big_Numbers.Big_Reals
    ("s-aoinar", T),  -- System.Atomic_Operations.Integer_Arithmetic
    ("s-aomoar", T),  -- System.Atomic_Operations.Modular_Arithmetic
    ("s-aotase", T),  -- System.Atomic_Operations.Test_And_Set
    ("s-atoope", T),  -- System.Atomic_Operations
    ("s-atopex", T),  -- System.Atomic_Operations.Exchange
    ("a-sttebu", T),  -- Ada.Strings.Text_Buffers
    ("a-stbuun", T),  -- Ada.Strings.Text_Buffers.Unbounded
    ("a-stbubo", T),  -- Ada.Strings.Text_Buffers.Bounded
    ("a-strsto", T),  -- Ada.Streams.Storage
    ("a-ststbo", T),  -- Ada.Streams.Storage.Bounded
    ("a-ststun", T),  -- Ada.Streams.Storage.Unbounded

   ----------------------------------------
   -- GNAT Defined Additions to Ada 2022 --
   ----------------------------------------

   ("a-stbufi", T),   -- Ada.Strings.Text_Buffers.Files
   ("a-stbufo", T),   -- Ada.Strings.Text_Buffers.Formatting
   ("a-stbuut", T)    -- Ada.Strings.Text_Buffers.Utils
   );

   -----------------------
   -- Alternative Units --
   -----------------------

   --  For some implementation units, there is a unit in the GNAT library
   --  that has identical functionality that is usable. If we have such a
   --  case we record the appropriate Unit name in Error_Msg_String.

   type Aunit_Record is record
      Fname : String (1 .. 6);
      Aname : String_Ptr_Const;
   end record;

   --  Array of alternative unit names

   Scasuti : aliased constant String := "GNAT.Case_Util";
   Scrc32  : aliased constant String := "GNAT.CRC32";
   Shtable : aliased constant String := "GNAT.HTable";
   Sos_lib : aliased constant String := "GNAT.OS_Lib";
   Sregexp : aliased constant String := "GNAT.Regexp";
   Sregpat : aliased constant String := "GNAT.Regpat";
   Sstring : aliased constant String := "GNAT.Strings";
   Sstusta : aliased constant String := "GNAT.Task_Stack_Usage";
   Stasloc : aliased constant String := "GNAT.Task_Lock";
   Sutf_32 : aliased constant String := "GNAT.UTF_32";

   --  Array giving mapping

   Map_Array : constant array (1 .. 10) of Aunit_Record := (
                 ("casuti", Scasuti'Access),
                 ("crc32 ", Scrc32 'Access),
                 ("htable", Shtable'Access),
                 ("os_lib", Sos_lib'Access),
                 ("regexp", Sregexp'Access),
                 ("regpat", Sregpat'Access),
                 ("string", Sstring'Access),
                 ("stusta", Sstusta'Access),
                 ("tasloc", Stasloc'Access),
                 ("utf_32", Sutf_32'Access));

   ----------------------
   -- Get_Kind_Of_File --
   ----------------------

   function Get_Kind_Of_File (File : String) return Kind_Of_Unit is
      pragma Assert (File'First = 1);

      Buffer : String (1 .. 9);

   begin
      Error_Msg_Strlen := 0;

      --  Ada/System/Interfaces are all Ada 95 units

      if File = "ada.ads"
        or else File = "interfac.ads"
        or else File = "system.ads"
      then
         return Ada_95_Unit;
      end if;

      --  Not predefined if file name does not start with a- g- s- i-

      if File'Length < 3
        or else File (2) /= '-'
        or else
          (File (1) /= 'a'
            and then File (1) /= 'g'
            and then File (1) /= 'i'
            and then File (1) /= 's')
      then
         return Not_Predefined_Unit;
      end if;

      --  If length of file name is greater than 12, not predefined. The value
      --  12 here is an 8 char name with extension .ads. The exception of 13 is
      --  for the implementation units of the 128-bit types under System.

      if File'Length > 12
        and then not (File'Length = 13 and then File (1) = 's')
      then
         return Not_Predefined_Unit;
      end if;

      --  Not predefined if file name does not end in .ads. This can happen
      --  when non-standard file names are being used.

      if Name_Buffer (Name_Len - 3 .. Name_Len) /= ".ads" then
         return Not_Predefined_Unit;
      end if;

      --  Otherwise normalize file name to 8 characters

      Buffer (1 .. File'Length - 4) := File (1 .. File'Length - 4);

      for J in File'Length - 3 .. 8 loop
         Buffer (J) := ' ';
      end loop;

      --  See if name is in 95 list

      for J in Non_Imp_File_Names_95'Range loop
         if Buffer (1 .. 8) = Non_Imp_File_Names_95 (J).Fname then
            return Ada_95_Unit;
         end if;
      end loop;

      --  See if name is in 2005 list

      for J in Non_Imp_File_Names_05'Range loop
         if Buffer (1 .. 8) = Non_Imp_File_Names_05 (J).Fname then
            return Ada_2005_Unit;
         end if;
      end loop;

      --  See if name is in 2012 list

      for J in Non_Imp_File_Names_12'Range loop
         if Buffer (1 .. 8) = Non_Imp_File_Names_12 (J).Fname then
            return Ada_2012_Unit;
         end if;
      end loop;

      --  See if name is in 2022 list

      for J in Non_Imp_File_Names_22'Range loop
         if Buffer (1 .. 8) = Non_Imp_File_Names_22 (J).Fname then
            return Ada_2022_Unit;
         end if;
      end loop;

      --  Only remaining special possibilities are children of System.RPC and
      --  System.Garlic and special files of the form System.Aux...

      if File (1 .. 5) = "s-aux"
        or else File (1 .. 5) = "s-gar"
        or else File (1 .. 5) = "s-rpc"
      then
         return Ada_95_Unit;
      end if;

      --  All tests failed, this is definitely an implementation unit. See if
      --  we have an alternative name.

      if File'Length in 11 .. 12
        and then File (1 .. 2) = "s-"
        and then File (File'Last - 3 .. File'Last) = ".ads"
      then
         for J in Map_Array'Range loop
            if (File'Length = 12 and then
                 File (3 .. 8) = Map_Array (J).Fname)
              or else
               (File'Length = 11 and then
                 File (3 .. 7) = Map_Array (J).Fname (1 .. 5))
            then
               Error_Msg_Strlen := Map_Array (J).Aname'Length;
               Error_Msg_String (1 .. Error_Msg_Strlen) :=
                 Map_Array (J).Aname.all;
            end if;
         end loop;
      end if;

      return Implementation_Unit;
   end Get_Kind_Of_File;

   ----------------------
   -- Get_Kind_Of_Unit --
   ----------------------

   function Get_Kind_Of_Unit (U : Unit_Number_Type) return Kind_Of_Unit is
   begin
      Get_Name_String (Unit_File_Name (U));
      return Get_Kind_Of_File (Name_Buffer (1 .. Name_Len));
   end Get_Kind_Of_Unit;

   -------------------
   -- Is_Known_Unit --
   -------------------

   function Is_Known_Unit (Nam : Node_Id) return Boolean is
      Unam : Unit_Name_Type;
      Fnam : File_Name_Type;

   begin
      --  If selector is not an identifier (e.g. it is a character literal or
      --  some junk from a previous error), then definitely not a known unit.

      if Nkind (Selector_Name (Nam)) /= N_Identifier then
         return False;
      end if;

      --  Otherwise get corresponding file name

      Unam := Get_Unit_Name (Nam);
      Fnam := Get_File_Name (Unam, Subunit => False);
      Get_Name_String (Fnam);

      Error_Msg_Strlen := 0;

      --  Ada/System/Interfaces are all Ada 95 units

      if (Name_Len =  7 and then Name_Buffer (1 ..  7) = "ada.ads")
           or else
         (Name_Len = 10 and then Name_Buffer (1 .. 10) = "system.ads")
           or else
         (Name_Len = 12 and then Name_Buffer (1 .. 12) = "interfac.ads")
      then
         return True;
      end if;

      --  Remove extension from file name

      if Name_Buffer (Name_Len - 3 .. Name_Len) = ".adb"
           or else
         Name_Buffer (Name_Len - 3 .. Name_Len) = ".ads"
      then
         Name_Len := Name_Len - 4;
      else
         return False;
      end if;

      --  Pad name to 8 characters

      while Name_Len < 8 loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ' ';
      end loop;

      --  If length more than 8, definitely not a match

      if Name_Len /= 8 then
         return False;
      end if;

      --  If length is 8, search our tables

      for J in Non_Imp_File_Names_95'Range loop
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names_95 (J).Fname then
            return True;
         end if;
      end loop;

      for J in Non_Imp_File_Names_05'Range loop
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names_05 (J).Fname then
            return True;
         end if;
      end loop;

      for J in Non_Imp_File_Names_12'Range loop
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names_12 (J).Fname then
            return True;
         end if;
      end loop;

      --  If not found, not known

      return False;

   --  A safety guard, if we get an exception during this processing then it
   --  is most likely the result of a previous error, or a peculiar case we
   --  have not thought of. Since this routine is only used for error message
   --  refinement, we will just return False.

   exception
      when others =>
         return False;
   end Is_Known_Unit;

   ---------------------------
   -- Not_Impl_Defined_Unit --
   ---------------------------

   function Not_Impl_Defined_Unit (U : Unit_Number_Type) return Boolean is
      Fname : constant File_Name_Type := Unit_File_Name (U);

   begin
      Error_Msg_Strlen := 0;
      Get_Name_String (Fname);

      --  Ada/System/Interfaces are all RM-defined Ada 95 units

      if (Name_Len =  7 and then Name_Buffer (1 ..  7) = "ada.ads")
           or else
         (Name_Len = 10 and then Name_Buffer (1 .. 10) = "system.ads")
           or else
         (Name_Len = 12 and then Name_Buffer (1 .. 12) = "interfac.ads")
      then
         return True;
      end if;

      --  Implementation defined if unit in the gnat hierarchy

      if (Name_Len = 8 and then Name_Buffer (1 .. 8) = "gnat.ads")
        or else (Name_Len > 2 and then Name_Buffer (1 .. 2) = "g-")
      then
         return False;
      end if;

      --  Not implementation defined if file name does not start with a- s- i-

      if Name_Len < 3
        or else Name_Buffer (2) /= '-'
        or else (Name_Buffer (1) /= 'a'
                   and then
                 Name_Buffer (1) /= 'i'
                   and then
                 Name_Buffer (1) /= 's')
      then
         return True;
      end if;

      --  If length of file name is greater than 12, not predefined. The value
      --  12 here is an 8 char name with extension .ads. The exception of 13 is
      --  for the implementation units of the 128-bit types under System.

      if Name_Len > 12
        and then not (Name_Len = 13 and then Name_Buffer (1) = 's')
      then
         return True;
      end if;

      --  Not impl-defined if file name does not end in .ads. This can happen
      --  when non-standard file names are being used.

      if Name_Buffer (Name_Len - 3 .. Name_Len) /= ".ads" then
         return True;
      end if;

      --  Otherwise normalize file name to 8 characters

      Name_Len := Name_Len - 4;
      while Name_Len < 8 loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ' ';
      end loop;

      --  Check our lists of names, if we find a match, return corresponding
      --  indication of whether the file is RM defined, respecting the RM
      --  version in which it is defined.

      for J in Non_Imp_File_Names_95'Range loop
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names_95 (J).Fname then
            return Non_Imp_File_Names_95 (J).RMdef;
         end if;
      end loop;

      for J in Non_Imp_File_Names_05'Range loop
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names_05 (J).Fname then
            return Non_Imp_File_Names_05 (J).RMdef
              and then Ada_Version >= Ada_2005;
         end if;
      end loop;

      for J in Non_Imp_File_Names_12'Range loop
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names_12 (J).Fname then
            return Non_Imp_File_Names_12 (J).RMdef
              and then Ada_Version >= Ada_2012;
         end if;
      end loop;

      --  If unit is in System, Ada or Interfaces hierarchies and did not match
      --  any entry in the list, means it is an internal implementation defined
      --  unit which the restriction should definition forbid.

      return True;
   end Not_Impl_Defined_Unit;

end Impunit;
