------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            C O N T R A C T S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2015, Free Software Foundation, Inc.            --
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

--  This package contains routines that perform analysis and expansion of
--  various contracts.

with Types; use Types;

package Contracts is

   procedure Add_Contract_Item (Prag : Node_Id; Id : Entity_Id);
   --  Add pragma Prag to the contract of a constant, entry, package [body],
   --  subprogram [body] or variable denoted by Id. The following are valid
   --  pragmas:
   --    Abstract_State
   --    Async_Readers
   --    Async_Writers
   --    Constant_After_Elaboration
   --    Contract_Cases
   --    Depends
   --    Effective_Reads
   --    Effective_Writes
   --    Extensions_Visible
   --    Global
   --    Initial_Condition
   --    Initializes
   --    Part_Of
   --    Postcondition
   --    Precondition
   --    Refined_Depends
   --    Refined_Global
   --    Refined_Post
   --    Refined_States
   --    Test_Case
   --    Volatile_Function

   procedure Analyze_Enclosing_Package_Body_Contract (Body_Decl : Node_Id);
   --  Analyze the contract of the nearest package body (if any) which encloses
   --  package or subprogram body Body_Decl.

   procedure Analyze_Object_Contract (Obj_Id : Entity_Id);
   --  Analyze all delayed pragmas chained on the contract of object Obj_Id as
   --  if they appeared at the end of the declarative region. The pragmas to be
   --  considered are:
   --    Async_Readers
   --    Async_Writers
   --    Effective_Reads
   --    Effective_Writes
   --    Part_Of

   procedure Analyze_Package_Body_Contract
     (Body_Id   : Entity_Id;
      Freeze_Id : Entity_Id := Empty);
   --  Analyze all delayed aspects chained on the contract of package body
   --  Body_Id as if they appeared at the end of a declarative region. The
   --  aspects that are considered are:
   --    Refined_State
   --
   --  Freeze_Id is the entity of a [generic] package body or a [generic]
   --  subprogram body which "feezes" the contract of Body_Id.

   procedure Analyze_Package_Contract (Pack_Id : Entity_Id);
   --  Analyze all delayed aspects chained on the contract of package Pack_Id
   --  as if they appeared at the end of a declarative region. The aspects
   --  that are considered are:
   --    Initial_Condition
   --    Initializes
   --    Part_Of

   procedure Analyze_Subprogram_Body_Contract (Body_Id : Entity_Id);
   --  Analyze all delayed aspects chained on the contract of subprogram body
   --  Body_Id as if they appeared at the end of a declarative region. Aspects
   --  in question are:
   --    Contract_Cases   (stand alone body)
   --    Depends          (stand alone body)
   --    Global           (stand alone body)
   --    Postcondition    (stand alone body)
   --    Precondition     (stand alone body)
   --    Refined_Depends
   --    Refined_Global
   --    Refined_Post
   --    Test_Case        (stand alone body)

   procedure Analyze_Subprogram_Contract (Subp_Id : Entity_Id);
   --  Analyze all delayed aspects chained on the contract of subprogram
   --  Subp_Id as if they appeared at the end of a declarative region. The
   --  aspects in question are:
   --    Contract_Cases
   --    Depends
   --    Global
   --    Postcondition
   --    Precondition
   --    Test_Case

   procedure Analyze_Subprogram_Body_Stub_Contract (Stub_Id : Entity_Id);
   --  Analyze all delayed aspects chained on the contract of a subprogram body
   --  stub Stub_Id as if they appeared at the end of a declarative region. The
   --  aspects in question are:
   --    Contract_Cases
   --    Depends
   --    Global
   --    Postcondition
   --    Precondition
   --    Refined_Depends
   --    Refined_Global
   --    Refined_Post
   --    Test_Case

   procedure Create_Generic_Contract (Unit : Node_Id);
   --  Create a contract node for a generic package, generic subprogram or a
   --  generic body denoted by Unit by collecting all source contract-related
   --  pragmas in the contract of the unit.

   procedure Inherit_Subprogram_Contract
     (Subp      : Entity_Id;
      From_Subp : Entity_Id);
   --  Inherit relevant contract items from source subprogram From_Subp. Subp
   --  denotes the destination subprogram. The inherited items are:
   --    Extensions_Visible
   --  ??? it would be nice if this routine handles Pre'Class and Post'Class

   procedure Instantiate_Subprogram_Contract (Templ : Node_Id; L : List_Id);
   --  Instantiate all source pragmas found in the contract of the generic
   --  subprogram declaration template denoted by Templ. The instantiated
   --  pragmas are added to list L.

   procedure Save_Global_References_In_Contract
     (Templ  : Node_Id;
      Gen_Id : Entity_Id);
   --  Save all global references found within the aspect specifications and
   --  the contract-related source pragmas assocated with generic template
   --  Templ. Gen_Id denotes the entity of the analyzed generic copy.

end Contracts;
