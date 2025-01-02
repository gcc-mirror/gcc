------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 9                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Table;
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
   procedure Analyze_Protected_Type_Declaration         (N : Node_Id);
   procedure Analyze_Requeue                            (N : Node_Id);
   procedure Analyze_Selective_Accept                   (N : Node_Id);
   procedure Analyze_Single_Protected_Declaration       (N : Node_Id);
   procedure Analyze_Single_Task_Declaration            (N : Node_Id);
   procedure Analyze_Task_Body                          (N : Node_Id);
   procedure Analyze_Task_Definition                    (N : Node_Id);
   procedure Analyze_Task_Type_Declaration              (N : Node_Id);
   procedure Analyze_Terminate_Alternative              (N : Node_Id);
   procedure Analyze_Timed_Entry_Call                   (N : Node_Id);
   procedure Analyze_Triggering_Alternative             (N : Node_Id);

   procedure Install_Declarations (Spec : Entity_Id);
   --  Make visible in corresponding body the entities defined in a task,
   --  protected type declaration, or entry declaration.

   ------------------------------
   -- Lock Free Data Structure --
   ------------------------------

   --  A lock-free subprogram is a protected routine which references a unique
   --  protected scalar component and does not contain statements that cause
   --  side effects. Due to this restricted behavior, all references to shared
   --  data from within the subprogram can be synchronized through the use of
   --  atomic operations rather than relying on locks.

   type Lock_Free_Subprogram is record
      Sub_Body : Node_Id;
      --  Reference to the body of a protected subprogram which meets the lock-
      --  free requirements.

      Comp_Id : Entity_Id;
      --  Reference to the scalar component referenced from within Sub_Body
   end record;

   --  This table establishes a relation between a protected subprogram body
   --  and a unique component it references. The table is used when building
   --  the lock-free versions of a protected subprogram body.

   package Lock_Free_Subprogram_Table is new Table.Table (
     Table_Component_Type => Lock_Free_Subprogram,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 5,
     Table_Increment      => 5,
     Table_Name           => "Lock_Free_Subprogram_Table");
end Sem_Ch9;
