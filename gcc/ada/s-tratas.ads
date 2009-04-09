------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . T R A C E S . T A S K I N G               --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2001-2009 Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software;  you can redistribute it  and/or modify it under --
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
