------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 9                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;

package Sem_Ch9  is
   procedure Analyze_Abort_Statement                    (N : Node_Id);
   procedure Analyze_Accept_Alternative                 (N : Node_Id);
   procedure Analyze_Accept_Statement                   (N : Node_Id);
   procedure Analyze_Asynchronous_Select                (N : Node_Id);
   procedure Analyze_Conditional_Entry_Call             (N : Node_Id);
   procedure Analyze_Delay_Alternative                  (N : Node_Id);
   procedure Analyze_Delay_Relative                     (N : Node_Id);
   procedure Analyze_Delay_Until                        (N : Node_Id);
   procedure Analyze_Entry_Body                         (N : Node_Id);
   procedure Analyze_Entry_Body_Formal_Part             (N : Node_Id);
   procedure Analyze_Entry_Call_Alternative             (N : Node_Id);
   procedure Analyze_Entry_Declaration                  (N : Node_Id);
   procedure Analyze_Entry_Index_Specification          (N : Node_Id);
   procedure Analyze_Protected_Body                     (N : Node_Id);
   procedure Analyze_Protected_Definition               (N : Node_Id);
   procedure Analyze_Protected_Type                     (N : Node_Id);
   procedure Analyze_Requeue                            (N : Node_Id);
   procedure Analyze_Selective_Accept                   (N : Node_Id);
   procedure Analyze_Single_Protected                   (N : Node_Id);
   procedure Analyze_Single_Task                        (N : Node_Id);
   procedure Analyze_Task_Body                          (N : Node_Id);
   procedure Analyze_Task_Definition                    (N : Node_Id);
   procedure Analyze_Task_Type                          (N : Node_Id);
   procedure Analyze_Terminate_Alternative              (N : Node_Id);
   procedure Analyze_Timed_Entry_Call                   (N : Node_Id);
   procedure Analyze_Triggering_Alternative             (N : Node_Id);
end Sem_Ch9;
