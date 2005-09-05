------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . T R A C E S . T A S K I N G               --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2001-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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

--  This package provides all procedures used to implement debug traces
--  in the case tasking is involved.

--  See System.Traces for an overview of the various files involved in Tracing

--  If tasking is not involved, refer to System.Traces.General

with System.Tasking;

package System.Traces.Tasking is
   pragma Preelaborate;

   package ST renames System.Tasking;

   --  Send_Trace_Info procedures

   --  They are overloaded, depending on the parameters passed with the event

   procedure Send_Trace_Info
     (Id         : Trace_T;
      Task_Name2 : ST.Task_Id);

   procedure Send_Trace_Info
     (Id           : Trace_T;
      Task_Name2   : ST.Task_Id;
      Entry_Number : ST.Entry_Index);

   procedure Send_Trace_Info
     (Id           : Trace_T;
      Task_Name    : ST.Task_Id;
      Task_Name2   : ST.Task_Id;
      Entry_Number : ST.Entry_Index);

   procedure Send_Trace_Info
     (Id         : Trace_T;
      Task_Name  : ST.Task_Id;
      Task_Name2 : ST.Task_Id);

   procedure Send_Trace_Info
     (Id           : Trace_T;
      Entry_Number : ST.Entry_Index);

   procedure Send_Trace_Info
     (Id           : Trace_T;
      Acceptor     : ST.Task_Id;
      Entry_Number : ST.Entry_Index;
      Timeout      : Duration);

   procedure Send_Trace_Info
     (Id           : Trace_T;
      Entry_Number : ST.Entry_Index;
      Timeout      : Duration);

   procedure Send_Trace_Info
     (Id         : Trace_T;
      Task_Name  : ST.Task_Id;
      Number     : Integer);

   procedure Send_Trace_Info
     (Id         : Trace_T;
      Task_Name  : ST.Task_Id;
      Number     : Integer;
      Timeout    : Duration);
end System.Traces.Tasking;
