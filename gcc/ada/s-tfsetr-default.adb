------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                     S Y S T E M . T R A C E S . S E N D                  --
--                                                                          --
--                                  B o d y                                 --
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

--  This version is for all targets, provided that System.IO.Put_Line is
--  functional. It prints debug information to Standard Output

with System.IO;   use System.IO;
with GNAT.Regpat; use GNAT.Regpat;

----------------
-- Send_Trace --
----------------

--  Prints debug information both in a human readable form
--  and in the form they are sent from upper layers.

separate (System.Traces.Format)
procedure Send_Trace (Id : Trace_T; Info : String) is

   type Param_Type is
     (Name_Param,
      Caller_Param,
      Entry_Param,
      Timeout_Param,
      Acceptor_Param,
      Parent_Param,
      Number_Param);
   --  Type of parameter found in the message

   Info_Trace : String_Trace := Format_Trace (Info);

   function Get_Param
     (Input    : String_Trace;
      Param    : Param_Type;
      How_Many : Integer)
      return     String;
   --  Extract a parameter from the given input string

   ---------------
   -- Get_Param --
   ---------------

   function Get_Param
     (Input    : String_Trace;
      Param    : Param_Type;
      How_Many : Integer)
      return     String
   is
      pragma Unreferenced (How_Many);

      Matches : Match_Array (1 .. 2);
   begin
      --  We need comments here ???

      case Param is
         when Name_Param     =>
            Match ("/N:([\w]+)", Input, Matches);

         when Caller_Param   =>
            Match ("/C:([\w]+)", Input, Matches);

         when Entry_Param =>
            Match ("/E:([\s]*) +([0-9 ,]+)", Input, Matches);

         when Timeout_Param =>
            Match ("/T:([\s]*) +([0-9]+.[0-9]+)", Input, Matches);

         when Acceptor_Param =>
            Match ("/A:([\w]+)", Input, Matches);

         when Parent_Param   =>
            Match ("/P:([\w]+)", Input, Matches);

         when Number_Param =>
            Match ("/#:([\s]*) +([0-9]+)", Input, Matches);
      end case;

      if Matches (1).First < Input'First then
         return "";
      end if;

      case Param is
         when Timeout_Param | Entry_Param | Number_Param =>
            return Input (Matches (2).First .. Matches (2).Last);

         when others =>
            return Input (Matches (1).First .. Matches (1).Last);
      end case;
   end Get_Param;

--  Start of processing for Send_Trace

begin
   New_Line;
   Put_Line ("- Trace Debug Info ----------------");
   Put ("Caught event Id : ");

   case Id is
      when M_Accept_Complete => Put ("M_Accept_Complete");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " completes accept on entry "
                   & Get_Param (Info_Trace, Entry_Param, 1) & " with "
                   & Get_Param (Info_Trace, Caller_Param, 1));

      when M_Select_Else     => Put ("M_Select_Else");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " selects else statement");

      when M_RDV_Complete    => Put ("M_RDV_Complete");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " completes rendezvous with "
                   & Get_Param (Info_Trace, Caller_Param, 1));

      when M_Call_Complete   => Put ("M_Call_Complete");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " completes call");

      when M_Delay           => Put ("M_Delay");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " completes delay "
                   & Get_Param (Info_Trace, Timeout_Param, 1));

      when E_Missed          => Put ("E_Missed");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " got an invalid acceptor "
                   & Get_Param (Info_Trace, Acceptor_Param, 1));

      when E_Timeout         => Put ("E_Timeout");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " ends select due to timeout ");

      when E_Kill            => Put ("E_Kill");
         New_Line;
         Put_Line ("Asynchronous Transfer of Control on task "
                   & Get_Param (Info_Trace, Name_Param, 1));

      when W_Delay           => Put ("W_Delay");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " sleeping "
                   & Get_Param (Info_Trace, Timeout_Param, 1)
                   & " seconds");

      when WU_Delay           => Put ("WU_Delay");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " sleeping until "
                   & Get_Param (Info_Trace, Timeout_Param, 1));

      when W_Call            => Put ("W_Call");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " calling entry "
                   & Get_Param (Info_Trace, Entry_Param, 1)
                   & " of "  & Get_Param (Info_Trace, Acceptor_Param, 1));

      when W_Accept          => Put ("W_Accept");
         New_Line;
         Put ("Task " & Get_Param (Info_Trace, Name_Param, 1)
              & " waiting on "
              & Get_Param (Info_Trace, Number_Param, 1)
              & " accept(s)"
              & ", " & Get_Param (Info_Trace, Entry_Param, 1));
         New_Line;

      when W_Select          => Put ("W_Select");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " waiting on "
                   & Get_Param (Info_Trace, Number_Param, 1)
                   & " select(s)"
                      & ", " & Get_Param (Info_Trace, Entry_Param, 1));
         New_Line;

      when W_Completion      => Put ("W_Completion");
         New_Line;
            Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                      & " waiting for completion ");

      when WT_Select         => Put ("WT_Select");
         New_Line;
         Put ("Task " & Get_Param (Info_Trace, Name_Param, 1)
              & " waiting " & Get_Param (Info_Trace, Timeout_Param, 1)
              & " seconds  on "
              & Get_Param (Info_Trace, Number_Param, 1)
              & " select(s)");

         if Get_Param (Info_Trace, Number_Param, 1) /= "" then
            Put (", " & Get_Param (Info_Trace, Entry_Param, 1));
         end if;

         New_Line;

      when WT_Call           => Put ("WT_Call");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " calling entry "
                   & Get_Param (Info_Trace, Entry_Param, 1)
                   & " of "  & Get_Param (Info_Trace, Acceptor_Param, 1)
                   & " with timeout "
                   & Get_Param (Info_Trace, Timeout_Param, 1));

      when WT_Completion     => Put ("WT_Completion");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " waiting "
                   & Get_Param (Info_Trace, Timeout_Param, 1)
                   & " for call completion");

      when PO_Call           => Put ("PO_Call");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " calling protected entry  "
                   & Get_Param (Info_Trace, Entry_Param, 1));

      when POT_Call          => Put ("POT_Call");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " calling protected entry  "
                   & Get_Param (Info_Trace, Entry_Param, 1)
                   & " with timeout "
                   & Get_Param (Info_Trace, Timeout_Param, 1));

      when PO_Run            => Put ("PO_Run");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                      & " running entry  "
                   & Get_Param (Info_Trace, Entry_Param, 1)
                   & " for "
                   & Get_Param (Info_Trace, Caller_Param, 1));

      when PO_Done           => Put ("PO_Done");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " finished call from "
                   & Get_Param (Info_Trace, Caller_Param, 1));

      when PO_Lock           => Put ("PO_Lock");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " took lock");

      when PO_Unlock         => Put ("PO_Unlock");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " released lock");

      when T_Create          => Put ("T_Create");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " created");

      when T_Activate        => Put ("T_Activate");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " activated");

      when T_Abort           => Put ("T_Abort");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " aborted by "
                   & Get_Param (Info_Trace, Parent_Param, 1));

      when T_Terminate       => Put ("T_Terminate");
         New_Line;
         Put_Line ("Task " & Get_Param (Info_Trace, Name_Param, 1)
                   & " terminated");

      when others
        => Put ("Invalid Id");
   end case;

   Put_Line ("  --> " & Info_Trace);
   Put_Line ("-----------------------------------");
   New_Line;
end Send_Trace;
