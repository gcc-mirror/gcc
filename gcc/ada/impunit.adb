------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I M P U N I T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2000-2004 Free Software Foundation, Inc.         --
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

with Lib;   use Lib;
with Namet; use Namet;
with Opt;   use Opt;

package body Impunit is

   subtype File_Name_8 is String (1 .. 8);
   type File_List is array (Nat range <>) of File_Name_8;

   --  The following is a giant string containing the concenated names
   --  of all non-implementation internal files, i.e. the complete list
   --  of files for internal units which a program may legitimately WITH.

   --  Note that this list should match the list of units documented in
   --  the "GNAT Library" section of the GNAT Reference Manual.

   Non_Imp_File_Names : constant File_List := (

   -----------------------------------------------
   -- Ada Hierarchy Units from Reference Manual --
   -----------------------------------------------

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
     "a-nltcty",    -- Ada.Numerics.Long_Long_Complex_Types
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

     "a-chlat9",    -- Ada.Characters.Latin_9
     "a-colien",    -- Ada.Command_Line.Environment
     "a-colire",    -- Ada.Command_Line.Remove
     "a-cwila1",    -- Ada.Characters.Wide_Latin_1
     "a-cwila9",    -- Ada.Characters.Wide_Latin_9
     "a-diocst",    -- Ada.Direct_IO.C_Streams
     "a-einuoc",    -- Ada.Exceptions.Is_Null_Occurrence
     "a-exctra",    -- Ada.Exceptions.Traceback
     "a-siocst",    -- Ada.Sequential_IO.C_Streams
     "a-ssicst",    -- Ada.Streams.Stream_IO.C_Streams
     "a-suteio",    -- Ada.Strings.Unbounded.Text_IO
     "a-swuwti",    -- Ada.Strings.Wide_Unbounded.Wide_Text_IO
     "a-taidim",    -- Ada.Task_Identification.Image
     "a-tiocst",    -- Ada.Text_IO.C_Streams
     "a-wtcstr",    -- Ada.Wide_Text_IO.C_Streams

   ---------------------------
   -- GNAT Special IO Units --
   ---------------------------

   --  As further explained elsewhere (see Sem_Ch10), the internal
   --  packages of Text_IO and Wide_Text_IO are actually implemented
   --  as separate children, but this fact is intended to be hidden
   --  from the user completely. Any attempt to WITH one of these
   --  units will be diagnosed as an error later on, but for now we
   --  do not consider these internal implementation units (if we did,
   --  then we would get a junk warning which would be confusing and
   --  unecessary, given that we generate a clear error message).

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

     "g-arrspl",    -- GNAT.Array_Split
     "g-awk   ",    -- GNAT.AWK
     "g-boubuf",    -- GNAT.Bounded_Buffers
     "g-boumai",    -- GNAT.Bounded_Mailboxes
     "g-bubsor",    -- GNAT.Bubble_Sort
     "g-busora",    -- GNAT.Bubble_Sort_A
     "g-busorg",    -- GNAT.Bubble_Sort_G
     "g-calend",    -- GNAT.Calendar
     "g-casuti",    -- GNAT.Case_Util
     "g-catiio",    -- GNAT.Calendar.Time_IO
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
     "g-diopit",    -- GNAT.Directory_Operations.Iteration
     "g-dirope",    -- GNAT.Directory_Operations
     "g-dynhta",    -- GNAT.Dynamic_HTables
     "g-dyntab",    -- GNAT.Dynamic_Tables
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
     "g-pehage",    -- GNAT.Perfect_Hash.Generators
     "g-perhas",    -- GNAT.Perfect_Hash
     "g-regexp",    -- GNAT.Regexp
     "g-regist",    -- GNAT.Registry
     "g-regpat",    -- GNAT.Regpat
     "g-semaph",    -- GNAT.Semaphores
     "g-sestin",    -- GNAT.Secondary_Stack_Info
     "g-signal",    -- GNAT.Signals
     "g-socket",    -- GNAT.Sockets
     "g-souinf",    -- GNAT.Source_Info
     "g-speche",    -- GNAT.Spell_Checker
     "g-spipat",    -- GNAT.Spitbol.Patterns
     "g-spitbo",    -- GNAT.Spitbol
     "g-sptabo",    -- GNAT.Spitbol.Table_Boolean
     "g-sptain",    -- GNAT.Spitbol.Table_Integer
     "g-sptavs",    -- GNAT.Spitbol.Table_Vstring
     "g-string",    -- GNAT.Strings
     "g-strspl",    -- GNAT.String_Split
     "g-table ",    -- GNAT.Table
     "g-tasloc",    -- GNAT.Task_Lock
     "g-thread",    -- GNAT.Threads
     "g-traceb",    -- GNAT.Traceback
     "g-trasym",    -- GNAT.Traceback.Symbolic
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
     "i-cpp   ",    -- Interfaces.CPP
     "i-cstrea",    -- Interfaces.C.Streams
     "i-jalaob",    -- Interfaces.Java.Lang.Object
     "i-jalasy",    -- Interfaces.Java.Lang.System
     "i-jalath",    -- Interfaces.Java.Lang.Thread
     "i-java  ",    -- Interfaces.Java
     "i-javlan",    -- Interfaces.Java.Lang
     "i-os2err",    -- Interfaces.Os2lib.Errors
     "i-os2lib",    -- Interfaces.Os2lib
     "i-os2syn",    -- Interfaces.Os2lib.Synchronization
     "i-os2thr",    -- Interfaces.Os2lib.Threads
     "i-pacdec",    -- Interfaces.Packed_Decimal
     "i-vthrea",    -- Interfaces.Vthreads
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
     "s-tasinf",    -- System.Task_Info
     "s-wchcnv",    -- System.Wch_Cnv
     "s-wchcon");   -- System.Wch_Con

   -------------------------
   -- Implementation_Unit --
   -------------------------

   function Implementation_Unit (U : Unit_Number_Type) return Boolean is
      Fname : constant File_Name_Type := Unit_File_Name (U);

   begin
      --  All units are OK in GNAT mode

      if GNAT_Mode then
         return False;
      end if;

      --  If length of file name is greater than 12, definitely OK!
      --  The value 12 here is an 8 char name with extension .ads.

      if Length_Of_Name (Fname) > 12 then
         return False;
      end if;

      --  Otherwise test file name

      Get_Name_String (Fname);

      --  Definitely OK if file name does not start with a- g- s- i-

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
         return False;
      end if;

      --  Definitely OK if file name does not end in .ads. This can
      --  happen when non-standard file names are being used.

      if Name_Buffer (Name_Len - 3 .. Name_Len) /= ".ads" then
         return False;
      end if;

      --  Otherwise normalize file name to 8 characters

      Name_Len := Name_Len - 4;
      while Name_Len < 8 loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ' ';
      end loop;

      --  Definitely OK if name is in list

      for J in Non_Imp_File_Names'Range loop
         if Name_Buffer (1 .. 8) = Non_Imp_File_Names (J) then
            return False;
         end if;
      end loop;

      --  Only remaining special possibilities are children of
      --  System.RPC and System.Garlic and special files of the
      --  form System.Aux...

      Get_Name_String (Unit_Name (U));

      if Name_Len > 12
        and then Name_Buffer (1 .. 11) = "system.rpc."
      then
         return False;
      end if;

      if Name_Len > 15
        and then Name_Buffer (1 .. 14) = "system.garlic."
      then
         return False;
      end if;

      if Name_Len > 11
        and then Name_Buffer (1 .. 10) = "system.aux"
      then
         return False;
      end if;

      --  All tests failed, this is definitely an implementation unit

      return True;

   end Implementation_Unit;

end Impunit;
