------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . T R A C E S . S E N D                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This version is for VxWorks targets.

--  Trace information is sent to WindView using the wvEvent function.

--  Note that wvEvent is from the VxWorks API.

--  When adding a new event, just give an Id to then event, and then modify
--  the WindView events database.

--  Refer to WindView User's Guide for more details on how to add new events
--  to the events database.

----------------
-- Send_Trace --
----------------

--  This procedure formats the string, maps the event Id to an Id
--  recognized by WindView, and send the event using wvEvent

separate (System.Traces.Format)
procedure Send_Trace (Id : Trace_T; Info : String) is

   procedure Wv_Event
     (Id : Integer;
      Buffer : System.Address;
      Size : Integer);
   pragma Import (C, Wv_Event, "wvEvent");

   Info_Trace : String_Trace;
   Id_Event   : Integer;

begin
   Info_Trace := Format_Trace (Info);

   case Id is
      when M_Accept_Complete => Id_Event := 30000;
      when M_Select_Else     => Id_Event := 30001;
      when M_RDV_Complete    => Id_Event := 30002;
      when M_Call_Complete   => Id_Event := 30003;
      when M_Delay           => Id_Event := 30004;
      when E_Kill            => Id_Event := 30005;
      when E_Missed          => Id_Event := 30006;
      when E_Timeout         => Id_Event := 30007;

      when W_Call            => Id_Event := 30010;
      when W_Accept          => Id_Event := 30011;
      when W_Select          => Id_Event := 30012;
      when W_Completion      => Id_Event := 30013;
      when W_Delay           => Id_Event := 30014;
      when WT_Select         => Id_Event := 30015;
      when WT_Call           => Id_Event := 30016;
      when WT_Completion     => Id_Event := 30017;
      when WU_Delay          => Id_Event := 30018;

      when PO_Call           => Id_Event := 30020;
      when POT_Call          => Id_Event := 30021;
      when PO_Run            => Id_Event := 30022;
      when PO_Lock           => Id_Event := 30023;
      when PO_Unlock         => Id_Event := 30024;
      when PO_Done           => Id_Event := 30025;

      when T_Create          => Id_Event := 30030;
      when T_Activate        => Id_Event := 30031;
      when T_Abort           => Id_Event := 30032;
      when T_Terminate       => Id_Event := 30033;

      --  Unrecognized events are given the special Id_Event value 29999

      when others            => Id_Event := 29999;

   end case;

   Wv_Event (Id_Event, Info_Trace'Address, Max_Size);
end Send_Trace;
