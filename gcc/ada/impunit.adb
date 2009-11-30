------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I M P U N I T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2000-2009, Free Software Foundation, Inc.        --
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

with Atree;    use Atree;
with Errout;   use Errout;
with Sinfo;    use Sinfo;
with Fname.UF; use Fname.UF;
with Lib;      use Lib;
with Namet;    use Namet;
with Uname;    use Uname;

--  Note: this package body is used by GPS and GNATBench to supply a list of
--  entries for help on available library routines.

package body Impunit is

   subtype File_Name_8 is String (1 .. 8);
   type File_List is array (Nat range <>) of File_Name_8;

   ------------------
   -- Ada 95 Units --
   ------------------

   --  The following is a giant string list containing the names of all non-
   --  implementation internal files, i.e. the complete list of files for
   --  internal units which a program may legitimately WITH when operating in
   --  either Ada 95 or Ada 05 mode.

   --  Note that this list should match the list of units documented in the
   --  "GNAT Library" section of the GNAT Reference Manual. A unit listed here
   --  must either be documented in that section or described in the Ada RM.

   Non_Imp_File_Names_95 : constant File_List := (

   ------------------------------------------------------
   -- Ada Hierarchy Units from Ada-83 Reference Manual --
   ------------------------------------------------------

     "a-astaco",    -- Ada.Asynchronous_Task_Control
     "a-calend",    -- Ada.Calendar
     "a-chahan",    -- Ada.Characters.Handling
     "a-charac",    -- Ada.Characters
     "a-chlat1",    -- Ada.Characters.Latin_1
     "a-comlin",    -- Ada.Command_Line
     "a-decima",    -- Ada.Decimal
     "a-direio",    -- Ada.Direct_IO
     "a-dynpri",    -- Ada.Dynamic_Priorities
     "a-except",    -- Ada.Exceptions
     "a-finali",    -- Ada.Finalization
     "a-flteio",    -- Ada.Float_Text_IO
     "a-fwteio",    -- Ada.Float_Wide_Text_IO
     "a-inteio",    -- Ada.Integer_Text_IO
     "a-interr",    -- Ada.Interrupts
     "a-intnam",    -- Ada.Interrupts.Names
     "a-ioexce",    -- Ada.IO_Exceptions
     "a-iwteio",    -- Ada.Integer_Wide_Text_IO
     "a-ncelfu",    -- Ada.Numerics.Complex_Elementary_Functions
     "a-ngcefu",    -- Ada.Numerics.Generic_Complex_Elementary_Functions
     "a-ngcoty",    -- Ada.Numerics.Generic_Complex_Types
     "a-ngelfu",    -- Ada.Numerics.Generic_Elementary_Functions
     "a-nucoty",    -- Ada.Numerics.Complex_Types
     "a-nudira",    -- Ada.Numerics.Discrete_Random
     "a-nuelfu",    -- Ada.Numerics.Elementary_Functions
     "a-nuflra",    -- Ada.Numerics.Float_Random
     "a-numeri",    -- Ada.Numerics
     "a-reatim",    -- Ada.Real_Time
     "a-sequio",    -- Ada.Sequential_IO
     "a-stmaco",    -- Ada.Strings.Maps.Constants
     "a-storio",    -- Ada.Storage_IO
     "a-strbou",    -- Ada.Strings.Bounded
     "a-stream",    -- Ada.Streams
     "a-strfix",    -- Ada.Strings.Fixed
     "a-string",    -- Ada.Strings
     "a-strmap",    -- Ada.Strings.Maps
     "a-strunb",    -- Ada.Strings.Unbounded
     "a-ststio",    -- Ada.Streams.Stream_IO
     "a-stwibo",    -- Ada.Strings.Wide_Bounded
     "a-stwifi",    -- Ada.Strings.Wide_Fixed
     "a-stwima",    -- Ada.Strings.Wide_Maps
     "a-stwiun",    -- Ada.Strings.Wide_Unbounded
     "a-swmwco",    -- Ada.Strings.Wide_Maps.Wide_Constants
     "a-sytaco",    -- Ada.Synchronous_Task_Control
     "a-tags  ",    -- Ada.Tags
     "a-tasatt",    -- Ada.Task_Attributes
     "a-taside",    -- Ada.Task_Identification
     "a-teioed",    -- Ada.Text_IO.Editing
     "a-textio",    -- Ada.Text_IO
     "a-ticoio",    -- Ada.Text_IO.Complex_IO
     "a-titest",    -- Ada.Text_IO.Text_Streams
     "a-unccon",    -- Ada.Unchecked_Conversion
     "a-uncdea",    -- Ada.Unchecked_Deallocation
     "a-witeio",    -- Ada.Wide_Text_IO
     "a-wtcoio",    -- Ada.Wide_Text_IO.Complex_IO
     "a-wtedit",    -- Ada.Wide_Text_IO.Editing
     "a-wttest",    -- Ada.Wide_Text_IO.Text_Streams

   -------------------------------------------------
   -- RM Required Additions to Ada for GNAT Types --
   -------------------------------------------------

     "a-lfteio",    -- Ada.Long_Float_Text_IO
     "a-lfwtio",    -- Ada.Long_Float_Wide_Text_IO
     "a-liteio",    -- Ada.Long_Integer_Text_IO
     "a-liwtio",    -- Ada.Long_Integer_Wide_Text_IO
     "a-llftio",    -- Ada.Long_Long_Float_Text_IO
     "a-llfwti",    -- Ada.Long_Long_Float_Wide_Text_IO
     "a-llitio",    -- Ada.Long_Long_Integer_Text_IO
     "a-lliwti",    -- Ada.Long_Long_Integer_Wide_Text_IO
     "a-nlcefu",    -- Ada.Long_Complex_Elementary_Functions
     "a-nlcoty",    -- Ada.Numerics.Long_Complex_Types
     "a-nlelfu",    -- Ada.Numerics.Long_Elementary_Functions
     "a-nllcef",    -- Ada.Long_Long_Complex_Elementary_Functions
     "a-nllefu",    -- Ada.Numerics.Long_Long_Elementary_Functions
     "a-nllcty",    -- Ada.Numerics.Long_Long_Complex_Types
     "a-nscefu",    -- Ada.Short_Complex_Elementary_Functions
     "a-nscoty",    -- Ada.Numerics.Short_Complex_Types
     "a-nselfu",    -- Ada.Numerics.Short_Elementary_Functions
     "a-sfteio",    -- Ada.Short_Float_Text_IO
     "a-sfwtio",    -- Ada.Short_Float_Wide_Text_IO
     "a-siteio",    -- Ada.Short_Integer_Text_IO
     "a-siwtio",    -- Ada.Short_Integer_Wide_Text_IO
     "a-ssitio",    -- Ada.Short_Short_Integer_Text_IO
     "a-ssiwti",    -- Ada.Short_Short_Integer_Wide_Text_IO

   -----------------------------------
   -- GNAT Defined Additions to Ada --
   -----------------------------------

     "a-calcon",    -- Ada.Calendar.Conversions
     "a-chlat9",    -- Ada.Characters.Latin_9
     "a-clrefi",    -- Ada.Command_Line.Response_File
     "a-colien",    -- Ada.Command_Line.Environment
     "a-colire",    -- Ada.Command_Line.Remove
     "a-cwila1",    -- Ada.Characters.Wide_Latin_1
     "a-cwila9",    -- Ada.Characters.Wide_Latin_9
     "a-diocst",    -- Ada.Direct_IO.C_Streams
     "a-einuoc",    -- Ada.Exceptions.Is_Null_Occurrence
     "a-elchha",    -- Ada.Exceptions.Last_Chance_Handler
     "a-exctra",    -- Ada.Exceptions.Traceback
     "a-siocst",    -- Ada.Sequential_IO.C_Streams
     "a-ssicst",    -- Ada.Streams.Stream_IO.C_Streams
     "a-suteio",    -- Ada.Strings.Unbounded.Text_IO
     "a-swuwti",    -- Ada.Strings.Wide_Unbounded.Wide_Text_IO
     "a-tiocst",    -- Ada.Text_IO.C_Streams
     "a-wtcstr",    -- Ada.Wide_Text_IO.C_Streams

      --  Note: strictly the next two should be Ada 2005 units, but it seems
      --  harmless (and useful) to make then available in Ada 95 mode, since
      --  they only deal with Wide_Character, not Wide_Wide_Character.

     "a-wichun",    -- Ada.Wide_Characters.Unicode
     "a-widcha",    -- Ada.Wide_Characters

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

     "a-tideio",    -- Ada.Text_IO.Decimal_IO
     "a-tienio",    -- Ada.Text_IO.Enumeration_IO
     "a-tifiio",    -- Ada.Text_IO.Fixed_IO
     "a-tiflio",    -- Ada.Text_IO.Float_IO
     "a-tiinio",    -- Ada.Text_IO.Integer_IO
     "a-tiinio",    -- Ada.Text_IO.Integer_IO
     "a-timoio",    -- Ada.Text_IO.Modular_IO
     "a-wtdeio",    -- Ada.Wide_Text_IO.Decimal_IO
     "a-wtenio",    -- Ada.Wide_Text_IO.Enumeration_IO
     "a-wtfiio",    -- Ada.Wide_Text_IO.Fixed_IO
     "a-wtflio",    -- Ada.Wide_Text_IO.Float_IO
     "a-wtinio",    -- Ada.Wide_Text_IO.Integer_IO
     "a-wtmoio",    -- Ada.Wide_Text_IO.Modular_IO

   ------------------------
   -- GNAT Library Units --
   ------------------------

     "g-altive",    -- GNAT.Altivec
     "g-altcon",    -- GNAT.Altivec.Conversions
     "g-alveop",    -- GNAT.Altivec.Vector_Operations
     "g-alvety",    -- GNAT.Altivec.Vector_Types
     "g-alvevi",    -- GNAT.Altivec.Vector_Views
     "g-arrspl",    -- GNAT.Array_Split
     "g-awk   ",    -- GNAT.AWK
     "g-boubuf",    -- GNAT.Bounded_Buffers
     "g-boumai",    -- GNAT.Bounded_Mailboxes
     "g-bubsor",    -- GNAT.Bubble_Sort
     "g-busora",    -- GNAT.Bubble_Sort_A
     "g-busorg",    -- GNAT.Bubble_Sort_G
     "g-byorma",    -- GNAT.Byte_Order_Mark
     "g-bytswa",    -- GNAT.Byte_Swapping
     "g-calend",    -- GNAT.Calendar
     "g-catiio",    -- GNAT.Calendar.Time_IO
     "g-casuti",    -- GNAT.Case_Util
     "g-cgi   ",    -- GNAT.CGI
     "g-cgicoo",    -- GNAT.CGI.Cookie
     "g-cgideb",    -- GNAT.CGI.Debug
     "g-comlin",    -- GNAT.Command_Line
     "g-comver",    -- GNAT.Compiler_Version
     "g-crc32 ",    -- GNAT.CRC32
     "g-ctrl_c",    -- GNAT.Ctrl_C
     "g-curexc",    -- GNAT.Current_Exception
     "g-debpoo",    -- GNAT.Debug_Pools
     "g-debuti",    -- GNAT.Debug_Utilities
     "g-decstr",    -- GNAT.Decode_String
     "g-deutst",    -- GNAT.Decode_UTF8_String
     "g-dirope",    -- GNAT.Directory_Operations
     "g-diopit",    -- GNAT.Directory_Operations.Iteration
     "g-dynhta",    -- GNAT.Dynamic_HTables
     "g-dyntab",    -- GNAT.Dynamic_Tables
     "g-encstr",    -- GNAT.Encode_String
     "g-enutst",    -- GNAT.Encode_UTF8_String
     "g-excact",    -- GNAT.Exception_Actions
     "g-except",    -- GNAT.Exceptions
     "g-exctra",    -- GNAT.Exception_Traces
     "g-expect",    -- GNAT.Expect
     "g-flocon",    -- GNAT.Float_Control
     "g-heasor",    -- GNAT.Heap_Sort
     "g-hesora",    -- GNAT.Heap_Sort_A
     "g-hesorg",    -- GNAT.Heap_Sort_G
     "g-htable",    -- GNAT.Htable
     "g-io    ",    -- GNAT.IO
     "g-io_aux",    -- GNAT.IO_Aux
     "g-locfil",    -- GNAT.Lock_Files
     "g-md5   ",    -- GNAT.MD5
     "g-memdum",    -- GNAT.Memory_Dump
     "g-moreex",    -- GNAT.Most_Recent_Exception
     "g-os_lib",    -- GNAT.Os_Lib
     "g-pehage",    -- GNAT.Perfect_Hash_Generators
     "g-rannum",    -- GNAT.Random_Numbers
     "g-regexp",    -- GNAT.Regexp
     "g-regist",    -- GNAT.Registry
     "g-regpat",    -- GNAT.Regpat
     "g-semaph",    -- GNAT.Semaphores
     "g-sercom",    -- GNAT.Serial_Communications
     "g-sestin",    -- GNAT.Secondary_Stack_Info
     "g-sha1  ",    -- GNAT.SHA1
     "g-sha224",    -- GNAT.SHA224
     "g-sha256",    -- GNAT.SHA256
     "g-sha384",    -- GNAT.SHA384
     "g-sha512",    -- GNAT.SHA512
     "g-signal",    -- GNAT.Signals
     "g-socket",    -- GNAT.Sockets
     "g-souinf",    -- GNAT.Source_Info
     "g-speche",    -- GNAT.Spell_Checker
     "g-spchge",    -- GNAT.Spell_Checker_Generic
     "g-spitbo",    -- GNAT.Spitbol
     "g-spipat",    -- GNAT.Spitbol.Patterns
     "g-sptabo",    -- GNAT.Spitbol.Table_Boolean
     "g-sptain",    -- GNAT.Spitbol.Table_Integer
     "g-sptavs",    -- GNAT.Spitbol.Table_Vstring
     "g-string",    -- GNAT.Strings
     "g-strspl",    -- GNAT.String_Split
     "g-sse   ",    -- GNAT.SSE
     "g-ssvety",    -- GNAT.SSE.Vector_Types
     "g-table ",    -- GNAT.Table
     "g-tasloc",    -- GNAT.Task_Lock
     "g-thread",    -- GNAT.Threads
     "g-timsta",    -- GNAT.Time_Stamp
     "g-traceb",    -- GNAT.Traceback
     "g-trasym",    -- GNAT.Traceback.Symbolic
     "g-utf_32",    -- GNAT.UTF_32
     "g-u3spch",    -- GNAT.UTF_32_Spelling_Checker
     "g-wispch",    -- GNAT.Wide_Spelling_Checker
     "g-wistsp",    -- GNAT.Wide_String_Split

   -----------------------------------------------------
   -- Interface Hierarchy Units from Reference Manual --
   -----------------------------------------------------

     "i-c     ",    -- Interfaces.C
     "i-cobol ",    -- Interfaces.Cobol
     "i-cpoint",    -- Interfaces.C.Pointers
     "i-cstrin",    -- Interfaces.C.Strings
     "i-fortra",    -- Interfaces.Fortran

   ------------------------------------------
   -- GNAT Defined Additions to Interfaces --
   ------------------------------------------

     "i-cexten",    -- Interfaces.C.Extensions
     "i-cil   ",    -- Interfaces.CIL
     "i-cilobj",    -- Interfaces.CIL.Object
     "i-cpp   ",    -- Interfaces.CPP
     "i-cstrea",    -- Interfaces.C.Streams
     "i-java  ",    -- Interfaces.Java
     "i-javjni",    -- Interfaces.Java.JNI
     "i-pacdec",    -- Interfaces.Packed_Decimal
     "i-vxwoio",    -- Interfaces.VxWorks.IO
     "i-vxwork",    -- Interfaces.VxWorks

   --------------------------------------------------
   -- System Hierarchy Units from Reference Manual --
   --------------------------------------------------

     "s-atacco",    -- System.Address_To_Access_Conversions
     "s-maccod",    -- System.Machine_Code
     "s-rpc   ",    -- System.Rpc
     "s-stoele",    -- System.Storage_Elements
     "s-stopoo",    -- System.Storage_Pools

   --------------------------------------
   -- GNAT Defined Additions to System --
   --------------------------------------

     "s-addima",    -- System.Address_Image
     "s-assert",    -- System.Assertions
     "s-memory",    -- System.Memory
     "s-parint",    -- System.Partition_Interface
     "s-pooglo",    -- System.Pool_Global
     "s-pooloc",    -- System.Pool_Local
     "s-restri",    -- System.Restrictions
     "s-rident",    -- System.Rident
     "s-ststop",    -- System.Strings.Stream_Ops
     "s-tasinf",    -- System.Task_Info
     "s-wchcnv",    -- System.Wch_Cnv
     "s-wchcon");   -- System.Wch_Con

   --------------------
   -- Ada 2005 Units --
   --------------------

   --  The following units should be used only in Ada 05 mode

   Non_Imp_File_Names_05 : constant File_List := (

   --------------------------------------------------------
   -- Ada Hierarchy Units from Ada 2005 Reference Manual --
   --------------------------------------------------------

     "a-assert",    -- Ada.Assertions
     "a-calari",    -- Ada.Calendar.Arithmetic
     "a-calfor",    -- Ada.Calendar.Formatting
     "a-catizo",    -- Ada.Calendar.Time_Zones
     "a-cdlili",    -- Ada.Containers.Doubly_Linked_Lists
     "a-cgarso",    -- Ada.Containers.Generic_Array_Sort
     "a-cgcaso",    -- Ada.Containers.Generic_Constrained_Array_Sort
     "a-chacon",    -- Ada.Characters.Conversions
     "a-cidlli",    -- Ada.Containers.Indefinite_Doubly_Linked_Lists
     "a-cihama",    -- Ada.Containers.Indefinite_Hashed_Maps
     "a-cihase",    -- Ada.Containers.Indefinite_Hashed_Sets
     "a-ciorma",    -- Ada.Containers.Indefinite_Ordered_Maps
     "a-ciorse",    -- Ada.Containers.Indefinite_Ordered_Sets
     "a-cohama",    -- Ada.Containers.Hashed_Maps
     "a-cohase",    -- Ada.Containers.Hashed_Sets
     "a-coinve",    -- Ada.Containers.Indefinite_Vectors
     "a-contai",    -- Ada.Containers
     "a-convec",    -- Ada.Containers.Vectors
     "a-coorma",    -- Ada.Containers.Ordered_Maps
     "a-coorse",    -- Ada.Containers.Ordered_Sets
     "a-coteio",    -- Ada.Complex_Text_IO
     "a-direct",    -- Ada.Directories
     "a-diroro",    -- Ada.Dispatching.Round_Robin
     "a-disedf",    -- Ada.Dispatching.EDF
     "a-dispat",    -- Ada.Dispatching
     "a-envvar",    -- Ada.Environment_Variables
     "a-exetim",    -- Ada.Execution_Time
     "a-extiti",    -- Ada.Execution_Time.Timers
     "a-rttiev",    -- Ada.Real_Time.Timing_Events
     "a-ngcoar",    -- Ada.Numerics.Generic_Complex_Arrays
     "a-ngrear",    -- Ada.Numerics.Generic_Real_Arrays
     "a-nucoar",    -- Ada.Numerics.Complex_Arrays
     "a-nurear",    -- Ada.Numerics.Real_Arrays
     "a-stboha",    -- Ada.Strings.Bounded.Hash
     "a-stfiha",    -- Ada.Strings.Fixed.Hash
     "a-strhas",    -- Ada.Strings.Hash
     "a-stunha",    -- Ada.Strings.Unbounded.Hash
     "a-stwiha",    -- Ada.Strings.Wide_Hash
     "a-stzbou",    -- Ada.Strings.Wide_Wide_Bounded
     "a-stzfix",    -- Ada.Strings.Wide_Wide_Fixed
     "a-stzhas",    -- Ada.Strings.Wide_Wide_Hash
     "a-stzmap",    -- Ada.Strings.Wide_Wide_Maps
     "a-stzunb",    -- Ada.Strings.Wide_Wide_Unbounded
     "a-swbwha",    -- Ada.Strings.Wide_Bounded.Wide_Hash
     "a-swfwha",    -- Ada.Strings.Wide_Fixed.Wide_Hash
     "a-swuwha",    -- Ada.Strings.Wide_Unbounded.Wide_Hash
     "a-szbzha",    -- Ada.Strings.Wide_Wide_Bounded.Wide_Wide_Hash
     "a-szfzha",    -- Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash
     "a-szmzco",    -- Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants
     "a-szuzha",    -- Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash
     "a-taster",    -- Ada.Task_Termination
     "a-tgdico",    -- Ada.Tags.Generic_Dispatching_Constructor
     "a-tiboio",    -- Ada.Text_IO.Bounded_IO
     "a-tiunio",    -- Ada.Text_IO.Unbounded_IO
     "a-wichun",    -- Ada.Wide_Characters.Unicode
     "a-wwboio",    -- Ada.Wide_Text_IO.Wide_Bounded_IO
     "a-wwunio",    -- Ada.Wide_Text_IO.Wide_Unbounded_IO
     "a-zchara",    -- Ada.Wide_Wide_Characters
     "a-ztcoio",    -- Ada.Wide_Wide_Text_IO.Complex_IO
     "a-ztedit",    -- Ada.Wide_Wide_Text_IO.Editing
     "a-zttest",    -- Ada.Wide_Wide_Text_IO.Text_Streams
     "a-ztexio",    -- Ada.Wide_Wide_Text_IO
     "a-zzboio",    -- Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO
     "a-zzunio",    -- Ada.Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO

   ------------------------------------------------------
   -- RM Required Additions to Ada 2005 for GNAT Types --
   ------------------------------------------------------

     "a-lcteio",    -- Ada.Long_Complex_Text_IO
     "a-lfztio",    -- Ada.Long_Float_Wide_Wide_Text_IO
     "a-liztio",    -- Ada.Long_Integer_Wide_Wide_Text_IO
     "a-llctio",    -- Ada.Long_Long_Complex_Text_IO
     "a-llfzti",    -- Ada.Long_Long_Float_Wide_Wide_Text_IO
     "a-llizti",    -- Ada.Long_Long_Integer_Wide_Wide_Text_IO
     "a-nlcoar",    -- Ada.Numerics.Long_Complex_Arrays
     "a-nllcar",    -- Ada.Numerics.Long_Long_Complex_Arrays
     "a-nllrar",    -- Ada.Numerics.Long_Long_Real_Arrays
     "a-nlrear",    -- Ada.Numerics.Long_Real_Arrays
     "a-scteio",    -- Ada.Short_Complex_Text_IO
     "a-sfztio",    -- Ada.Short_Float_Wide_Wide_Text_IO
     "a-siztio",    -- Ada.Short_Integer_Wide_Wide_Text_IO
     "a-ssizti",    -- Ada.Short_Short_Integer_Wide_Wide_Text_IO
     "a-ztcstr",    -- Ada.Wide_Wide_Text_IO.C_Streams

   ----------------------------------------
   -- GNAT Defined Additions to Ada 2005 --
   ----------------------------------------

     "a-cgaaso",    -- Ada.Containers.Generic_Anonymous_Array_Sort
     "a-chzla1",    -- Ada.Characters.Wide_Wide_Latin_1
     "a-chzla9",    -- Ada.Characters.Wide_Wide_Latin_9
     "a-ciormu",    -- Ada.Containers.Indefinite_Ordered_Multisets
     "a-coormu",    -- Ada.Containers.Ordered_Multisets
     "a-crdlli",    -- Ada.Containers.Restricted_Doubly_Linked_Lists
     "a-secain",    -- Ada.Strings.Equal_Case_Insensitive
     "a-shcain",    -- Ada.Strings.Hash_Case_Insensitive
     "a-slcain",    -- Ada.Strings.Less_Case_Insensitive
     "a-szuzti",    -- Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO
     "a-zchuni",    -- Ada.Wide_Wide_Characters.Unicode

   ---------------------------
   -- GNAT Special IO Units --
   ---------------------------

   --  See Ada 95 section for further information. These packages are for the
   --  implementation of the Wide_Wide_Text_IO generic packages.

     "a-ztdeio",    -- Ada.Wide_Wide_Text_IO.Decimal_IO
     "a-ztenio",    -- Ada.Wide_Wide_Text_IO.Enumeration_IO
     "a-ztfiio",    -- Ada.Wide_Wide_Text_IO.Fixed_IO
     "a-ztflio",    -- Ada.Wide_Wide_Text_IO.Float_IO
     "a-ztinio",    -- Ada.Wide_Wide_Text_IO.Integer_IO
     "a-ztmoio",    -- Ada.Wide_Wide_Text_IO.Modular_IO

   ------------------------
   -- GNAT Library Units --
   ------------------------

     "g-zspche",    -- GNAT.Wide_Wide_Spelling_Checker
     "g-zstspl");   -- GNAT.Wide_Wide_String_Split

   -----------------------
   -- Alternative Units --
   -----------------------

   --  For some implementation units, there is a unit in the GNAT library
   --  that has identical functionality that is usable. If we have such a
   --  case we record the appropriate Unit name in Error_Msg_String.

   type Aunit_Record is record
      Fname : String (1 .. 6);
      Aname : String_Ptr;
   end record;

   --  Array of alternative unit names

   Scasuti : aliased String := "GNAT.Case_Util";
   Sos_lib : aliased String := "GNAT.OS_Lib";
   Sregexp : aliased String := "GNAT.Regexp";
   Sregpat : aliased String := "GNAT.Regpat";
   Sstring : aliased String := "GNAT.Strings";
   Sstusta : aliased String := "GNAT.Task_Stack_Usage";
   Stasloc : aliased String := "GNAT.Task_Lock";
   Sutf_32 : aliased String := "GNAT.UTF_32";

   --  Array giving mapping

   Map_Array : constant array (1 .. 8) of Aunit_Record := (
                 ("casuti", Scasuti'Access),
                 ("os_lib", Sos_lib'Access),
                 ("regexp", Sregexp'Access),
                 ("regpat", Sregpat'Access),
                 ("string", Sstring'Access),
                 ("stusta", Sstusta'Access),
                 ("tasloc", Stasloc'Access),
                 ("utf_32", Sutf_32'Access));

   ----------------------
   -- Get_Kind_Of_Unit --
   ----------------------

   function Get_Kind_Of_Unit (U : Unit_Number_Type) return Kind_Of_Unit is
      Fname : constant File_Name_Type := Unit_File_Name (U);

   begin
      Error_Msg_Strlen := 0;

      --  If length of file name is greater than 12, not predefined.
      --  The value 12 here is an 8 char name with extension .ads.

      if Length_Of_Name (Fname) > 12 then
         return Not_Predefined_Unit;
      end if;

      --  Otherwise test file name

      Get_Name_String (Fname);

      --  Not predefined if file name does not start with a- g- s- i-

      if Name_Len < 3
        or else Name_Buffer (2) /= '-'
        or else (Name_Buffer (1) /= 'a'
                   and then
                 Name_Buffer (1) /= 'g'
                   and then
                 Name_Buffer (1) /= 'i'
                   and then
                 Name_Buffer (1) /= 's')
      then
         return Not_Predefined_Unit;
      end if;

      --  Not predefined if file name does not end in .ads. This can
      --  happen when non-standard file names are being used.

      if Name_Buffer (Name_Len - 3 .. Name_Len) /= ".ads" then
         return Not_Predefined_Unit;
      end if;

      --  Otherwise normalize file name to 8 characters

      Name_Len := Name_Len - 4;
      while Name_Len < 8 loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ' ';
      end loop;

      --  See if name is in 95 list

      for J in Non_Imp_File_Names_95'Range loop
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names_95 (J) then
            return Ada_95_Unit;
         end if;
      end loop;

      --  See if name is in 05 list

      for J in Non_Imp_File_Names_05'Range loop
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names_05 (J) then
            return Ada_05_Unit;
         end if;
      end loop;

      --  Only remaining special possibilities are children of System.RPC and
      --  System.Garlic and special files of the form System.Aux...

      Get_Name_String (Unit_Name (U));

      if Name_Len > 12
        and then Name_Buffer (1 .. 11) = "system.rpc."
      then
         return Ada_95_Unit;
      end if;

      if Name_Len > 15
        and then Name_Buffer (1 .. 14) = "system.garlic."
      then
         return Ada_95_Unit;
      end if;

      if Name_Len > 11
        and then Name_Buffer (1 .. 10) = "system.aux"
      then
         return Ada_95_Unit;
      end if;

      --  All tests failed, this is definitely an implementation unit. See if
      --  we have an alternative name.

      Get_Name_String (Fname);

      if Name_Len = 12
        and then Name_Buffer (1 .. 2) = "s-"
        and then Name_Buffer (9 .. 12) = ".ads"
      then
         for J in Map_Array'Range loop
            if Name_Buffer (3 .. 8) = Map_Array (J).Fname then
               Error_Msg_Strlen := Map_Array (J).Aname'Length;
               Error_Msg_String (1 .. Error_Msg_Strlen) :=
                 Map_Array (J).Aname.all;
            end if;
         end loop;
      end if;

      return Implementation_Unit;
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

      --  Remove extension from file name

      if Name_Buffer (Name_Len - 3 .. Name_Len) = ".adb" then
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
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names_95 (J) then
            return True;
         end if;
      end loop;

      for J in Non_Imp_File_Names_05'Range loop
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names_05 (J) then
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

end Impunit;
