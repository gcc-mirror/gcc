------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . G E N _ T C B I N F    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 1999-2000 Free Software Fundation             --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is an SGI Irix version of this package

--  This procedure creates the file "a-tcbinf.c"
--  "A-tcbinf.c" is subsequently compiled and made part of the RTL
--  to be referenced by the SGI Workshop debugger. The main procedure:
--  "Gen_Tcbinf" imports this child procedure and runs as part of the
--  RTL build process. Because of the complex process used to build
--  the GNAT RTL for all the different systems and the frequent changes
--  made to the internal data structures, its impractical to create
--  "a-tcbinf.c" using a standalone process.
with System.Tasking;
with Ada.Text_IO;
with Unchecked_Conversion;

procedure System.Task_Primitives.Gen_Tcbinf is

   use System.Tasking;

   subtype Version_String is String (1 .. 4);

   Version : constant Version_String := "3.11";

   function To_Integer is new Unchecked_Conversion
     (Version_String, Integer);

   type Dummy_TCB_Ptr is access Ada_Task_Control_Block (Entry_Num => 0);
   Dummy_TCB : constant Dummy_TCB_Ptr := new Ada_Task_Control_Block (0);

   C_File : Ada.Text_IO.File_Type;

   procedure Pl (S : String);
   procedure Nl (C : Ada.Text_IO.Positive_Count := 1);
   function State_Name (S : Task_States) return String;

   procedure Pl (S : String) is
   begin
      Ada.Text_IO.Put_Line (C_File, S);
   end Pl;

   procedure Nl (C : Ada.Text_IO.Positive_Count := 1) is
   begin
      Ada.Text_IO.New_Line (C_File, C);
   end Nl;

   function State_Name (S : Task_States) return String is
   begin
      case S is
         when Unactivated =>
            return "Unactivated";
         when Runnable =>
            return "Runnable";
         when Terminated =>
            return "Terminated";
         when Activator_Sleep =>
            return "Child Activation Wait";
         when Acceptor_Sleep =>
            return "Accept/Select Wait";
         when Entry_Caller_Sleep =>
            return "Waiting on Entry Call";
         when Async_Select_Sleep =>
            return "Async_Select Wait";
         when Delay_Sleep =>
            return "Delay Sleep";
         when Master_Completion_Sleep =>
            return "Child Termination Wait";
         when Master_Phase_2_Sleep =>
            return "Wait Child in Term Alt";
         when Interrupt_Server_Idle_Sleep =>
            return "Int Server Idle Sleep";
         when Interrupt_Server_Blocked_Interrupt_Sleep =>
            return "Int Server Blk Int Sleep";
         when Timer_Server_Sleep =>
            return "Timer Server Sleep";
         when AST_Server_Sleep =>
            return "AST Server Sleep";
         when Asynchronous_Hold =>
            return "Asynchronous Hold";
         when Interrupt_Server_Blocked_On_Event_Flag =>
            return "Int Server Blk Evt Flag";
      end case;
   end State_Name;

   All_Tasks_Link_Offset   : constant Integer
     := Dummy_TCB.Common'Position + Dummy_TCB.Common.All_Tasks_Link'Position;
   Entry_Count_Offset      : constant Integer
     := Dummy_TCB.Entry_Num'Position;
   Entry_Point_Offset      : constant Integer
     := Dummy_TCB.Common'Position + Dummy_TCB.Common.Task_Entry_Point'Position;
   Parent_Offset           : constant Integer
     := Dummy_TCB.Common'Position + Dummy_TCB.Common.Parent'Position;
   Base_Priority_Offset    : constant Integer
     := Dummy_TCB.Common'Position + Dummy_TCB.Common.Base_Priority'Position;
   Current_Priority_Offset : constant Integer
     := Dummy_TCB.Common'Position + Dummy_TCB.Common.Current_Priority'Position;
   Stack_Size_Offset       : constant Integer
     := Dummy_TCB.Common'Position +
       Dummy_TCB.Common.Compiler_Data.Pri_Stack_Info.Size'Position;
   State_Offset            : constant Integer
     := Dummy_TCB.Common'Position + Dummy_TCB.Common.State'Position;
   Task_Image_Offset       : constant Integer
     := Dummy_TCB.Common'Position + Dummy_TCB.Common.Task_Image'Position;
   Thread_Offset           : constant Integer
     := Dummy_TCB.Common'Position + Dummy_TCB.Common.LL'Position +
        Dummy_TCB.Common.LL.Thread'Position;

begin

   Ada.Text_IO.Create (C_File, Ada.Text_IO.Out_File, "a-tcbinf.c");

   Pl ("");
   Pl ("#include <sys/types.h>");
   Pl ("");
   Pl ("#define TCB_INFO_VERSION 2");
   Pl ("#define TCB_LIBRARY_VERSION "
     & Integer'Image (To_Integer (Version)));
   Pl ("");
   Pl ("typedef struct {");
   Pl ("");
   Pl ("   __uint32_t   info_version;");
   Pl ("   __uint32_t   library_version;");
   Pl ("");
   Pl ("   __uint32_t   All_Tasks_Link_Offset;");
   Pl ("   __uint32_t   Entry_Count_Offset;");
   Pl ("   __uint32_t   Entry_Point_Offset;");
   Pl ("   __uint32_t   Parent_Offset;");
   Pl ("   __uint32_t   Base_Priority_Offset;");
   Pl ("   __uint32_t   Current_Priority_Offset;");
   Pl ("   __uint32_t   Stack_Size_Offset;");
   Pl ("   __uint32_t   State_Offset;");
   Pl ("   __uint32_t   Task_Image_Offset;");
   Pl ("   __uint32_t   Thread_Offset;");
   Pl ("");
   Pl ("   char         **state_names;");
   Pl ("   __uint32_t   state_names_max;");
   Pl ("");
   Pl ("} task_control_block_info_t;");
   Pl ("");
   Pl ("static char *accepting_state_names = NULL;");

   Pl ("");
   Pl ("static char *task_state_names[] = {");

   for State in Task_States loop
      Pl ("   """ & State_Name (State) & """,");
   end loop;
   Pl ("   """"};");

   Pl ("");
   Pl ("");
   Pl ("task_control_block_info_t __task_control_block_info = {");
   Pl ("");
   Pl ("   TCB_INFO_VERSION,");
   Pl ("   TCB_LIBRARY_VERSION,");
   Pl ("");
   Pl ("   " & All_Tasks_Link_Offset'Img & ",");
   Pl ("   " & Entry_Count_Offset'Img & ",");
   Pl ("   " & Entry_Point_Offset'Img & ",");
   Pl ("   " & Parent_Offset'Img & ",");
   Pl ("   " & Base_Priority_Offset'Img & ",");
   Pl ("   " & Current_Priority_Offset'Img & ",");
   Pl ("   " & Stack_Size_Offset'Img & ",");
   Pl ("   " & State_Offset'Img & ",");
   Pl ("   " & Task_Image_Offset'Img & ",");
   Pl ("   " & Thread_Offset'Img & ",");
   Pl ("");
   Pl ("   task_state_names,");
   Pl ("   sizeof (task_state_names),");
   Pl ("");
   Pl ("");
   Pl ("};");

   Ada.Text_IO.Close (C_File);

end System.Task_Primitives.Gen_Tcbinf;
