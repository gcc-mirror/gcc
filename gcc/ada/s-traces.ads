------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                         S Y S T E M . T R A C E S                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2001-2014, Free Software Foundation, Inc.         --
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

--  This package implements functions for traces when tasking is not involved

--  Warning : NO dependencies to tasking should be created here

--  This package and all its children are used to implement debug information

--  A new primitive, Send_Trace_Info (Id : Trace_T; 'data') is introduced.
--  Trace_T is an event identifier, 'data' are the information to pass
--  with the event. This procedure is used from within the Runtime to send
--  debug information.

--  This primitive is overloaded in System.Traces.Tasking and this package

--  Send_Trace_Info calls Send_Trace, in System.Traces.Send, which is target
--  dependent, to send the debug information to a debugger, stream ..

--  To add a new event, just add them to the Trace_T type, and write the
--  corresponding Send_Trace_Info procedure. It may be required for some
--  target to modify Send_Trace (e.g. VxWorks).

--  To add a new target, just adapt System.Traces.Send as needed

package System.Traces is
   pragma Preelaborate;

   type Trace_T is
     (
      --  Events handled

      --  Messages

      M_Accept_Complete,
      M_Select_Else,
      M_RDV_Complete,
      M_Call_Complete,
      M_Delay,

      --  Errors

      E_Missed,
      E_Timeout,
      E_Kill,

      --  Waiting events

      W_Call,
      W_Accept,
      W_Select,
      W_Completion,
      W_Delay,
      WU_Delay,

      WT_Call,
      WT_Select,
      WT_Completion,

      --  Protected objects events

      PO_Call,
      POT_Call,
      PO_Run,
      PO_Lock,
      PO_Unlock,
      PO_Done,

      --  Task handling events

      T_Create,
      T_Activate,
      T_Abort,
      T_Terminate);

   --  Send_Trace_Info procedures

   --  They are overloaded, depending on the parameters passed with
   --  the event, e.g. Time information, Task name, Accept name ...

   procedure Send_Trace_Info (Id : Trace_T);

   procedure Send_Trace_Info (Id : Trace_T; Timeout : Duration);

end System.Traces;
