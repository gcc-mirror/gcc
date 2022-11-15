------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            C O N T R A C T S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2015-2022, Free Software Foundation, Inc.         --
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
   --  Add pragma Prag to the contract of a constant, entry, entry family,
   --  [generic] package, package body, protected unit, [generic] subprogram,
   --  subprogram body, variable, task unit, or type denoted by Id.
   --  The following are valid pragmas:
   --
   --    Abstract_State
   --    Async_Readers
   --    Async_Writers
   --    Attach_Handler
   --    Constant_After_Elaboration
   --    Contract_Cases
   --    Depends
   --    Effective_Reads
   --    Effective_Writes
   --    Extensions_Visible
   --    Global
   --    Initial_Condition
   --    Initializes
   --    Interrupt_Handler
   --    No_Caching
   --    Part_Of
   --    Postcondition
   --    Precondition
   --    Refined_Depends
   --    Refined_Global
   --    Refined_Post
   --    Refined_States
   --    Test_Case
   --    Volatile_Function

   procedure Analyze_Contracts (L : List_Id);
   --  Analyze the contracts of all eligible constructs found in list L

   procedure Analyze_Pragmas_In_Declarations (Body_Id : Entity_Id);
   --  Perform early analysis of pragmas at the top of a given subprogram's
   --  declarations.
   --
   --  The purpose of this is to analyze contract-related pragmas for later
   --  processing, but also to handle other such pragmas before these
   --  declarations get moved to an internal wrapper as part of contract
   --  expansion. For example, pragmas Inline, Ghost, Volatile all need to
   --  apply directly to the subprogram and not be moved to a wrapper.

   procedure Analyze_Entry_Or_Subprogram_Body_Contract (Body_Id : Entity_Id);
   --  Analyze all delayed pragmas chained on the contract of entry or
   --  subprogram body Body_Id as if they appeared at the end of a declarative
   --  region. Pragmas in question are:
   --
   --    Contract_Cases     (stand alone subprogram body)
   --    Depends            (stand alone subprogram body)
   --    Global             (stand alone subprogram body)
   --    Postcondition      (stand alone subprogram body)
   --    Precondition       (stand alone subprogram body)
   --    Refined_Depends
   --    Refined_Global
   --    Refined_Post
   --    Subprogram_Variant (stand alone subprogram body)
   --    Test_Case          (stand alone subprogram body)

   procedure Analyze_Entry_Or_Subprogram_Contract
     (Subp_Id   : Entity_Id;
      Freeze_Id : Entity_Id := Empty);
   --  Analyze all delayed pragmas chained on the contract of entry or
   --  subprogram Subp_Id as if they appeared at the end of a declarative
   --  region. The pragmas in question are:
   --
   --    Contract_Cases
   --    Depends
   --    Global
   --    Postcondition
   --    Precondition
   --    Subprogram_Variant
   --    Test_Case
   --
   --  Freeze_Id is the entity of a [generic] package body or a [generic]
   --  subprogram body which "freezes" the contract of Subp_Id.

   procedure Analyze_Object_Contract
     (Obj_Id    : Entity_Id;
      Freeze_Id : Entity_Id := Empty);
   --  Analyze all delayed pragmas chained on the contract of object Obj_Id as
   --  if they appeared at the end of the declarative region. The pragmas to be
   --  considered are:
   --
   --    Async_Readers
   --    Async_Writers
   --    Depends           (single concurrent object)
   --    Effective_Reads
   --    Effective_Writes
   --    Global            (single concurrent object)
   --    Part_Of
   --
   --  Freeze_Id is the entity of a [generic] package body or a [generic]
   --  subprogram body which "freezes" the contract of Obj_Id.

   procedure Analyze_Type_Contract (Type_Id : Entity_Id);
   --  Analyze all delayed pragmas chained on the contract of object Obj_Id as
   --  if they appeared at the end of the declarative region. The pragmas to be
   --  considered are:
   --
   --    Async_Readers
   --    Async_Writers
   --    Effective_Reads
   --    Effective_Writes
   --
   --  In the case of a protected or task type, there will also be
   --  a call to Analyze_Protected_Contract or Analyze_Task_Contract.

   procedure Analyze_Package_Body_Contract
     (Body_Id   : Entity_Id;
      Freeze_Id : Entity_Id := Empty);
   --  Analyze all delayed pragmas chained on the contract of package body
   --  Body_Id as if they appeared at the end of a declarative region. The
   --  pragmas that are considered are:
   --
   --    Refined_State
   --
   --  Freeze_Id is the entity of a [generic] package body or a [generic]
   --  subprogram body which "freezes" the contract of Body_Id.

   procedure Analyze_Package_Contract (Pack_Id : Entity_Id);
   --  Analyze all delayed pragmas chained on the contract of package Pack_Id
   --  as if they appeared at the end of a declarative region. The pragmas
   --  that are considered are:
   --
   --    Initial_Condition
   --    Initializes

   procedure Analyze_Protected_Contract (Prot_Id : Entity_Id);
   --  Analyze all delayed pragmas chained on the contract of protected unit
   --  Prot_Id if they appeared at the end of a declarative region. Currently
   --  there are no such pragmas.

   procedure Analyze_Subprogram_Body_Stub_Contract (Stub_Id : Entity_Id);
   --  Analyze all delayed pragmas chained on the contract of subprogram body
   --  stub Stub_Id as if they appeared at the end of a declarative region. The
   --  pragmas in question are:
   --
   --    Contract_Cases
   --    Depends
   --    Global
   --    Postcondition
   --    Precondition
   --    Refined_Depends
   --    Refined_Global
   --    Refined_Post
   --    Test_Case

   procedure Analyze_Task_Contract (Task_Id : Entity_Id);
   --  Analyze all delayed pragmas chained on the contract of task unit Task_Id
   --  as if they appeared at the end of a declarative region. The pragmas in
   --  question are:
   --
   --    Depends
   --    Global

   procedure Build_Entry_Contract_Wrapper (E : Entity_Id; Decl : Node_Id);
   --  Build the body of a wrapper procedure for an entry or entry family that
   --  has contract cases, preconditions, or postconditions, and add it to the
   --  freeze actions of the related synchronized type.
   --
   --  The body first verifies the preconditions and case guards of the
   --  contract cases, then invokes the entry [family], and finally verifies
   --  the postconditions and the consequences of the contract cases. E denotes
   --  the entry family. Decl denotes the declaration of the enclosing
   --  synchronized type.

   procedure Create_Generic_Contract (Unit : Node_Id);
   --  Create a contract node for a generic package, generic subprogram, or a
   --  generic body denoted by Unit by collecting all source contract-related
   --  pragmas in the contract of the unit.

   procedure Freeze_Previous_Contracts (Body_Decl : Node_Id);
   --  Freeze the contracts of all source constructs found in the declarative
   --  list which contains entry, package, protected, subprogram, or task body
   --  denoted by Body_Decl. In addition, freeze the contract of the nearest
   --  enclosing package body.

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

   procedure Make_Class_Precondition_Subps
     (Subp_Id         : Entity_Id;
      Late_Overriding : Boolean := False);
   --  Build helpers that at run time evaluate statically and dynamically the
   --  class-wide preconditions of Subp_Id; build also the indirect-call
   --  wrapper (ICW) required to check class-wide preconditions when the
   --  subprogram is invoked through an access-to-subprogram, or when it
   --  overrides an inherited class-wide precondition (see AI12-0195-1).
   --  Late_Overriding enables special handling required for late-overriding
   --  subprograms.
   --
   --  For example, if we have a subprogram with the following profile:
   --
   --     procedure Prim (Obj : TagTyp; <additional formals>)
   --       with Pre'Class => F1 (Obj) and F2(Obj)
   --
   --  We build the following helper that evaluates statically the class-wide
   --  precondition:
   --
   --    function PrimSP (Obj : TagTyp) return Boolean is
   --    begin
   --       return F1 (Obj) and F2(Obj);
   --    end PrimSP;
   --
   --   ... and the following helper that evaluates dynamically the class-wide
   --   precondition:
   --
   --    function PrimDP (Obj : TagTyp'Class; ...) return Boolean is
   --    begin
   --       return F1 (Obj) and F2(Obj);
   --    end PrimSP;
   --
   --   ... and the following indirect-call wrapper (ICW) that is used by the
   --   code generated by the compiler for indirect calls:
   --
   --    procedure PrimICW (Obj : TagTyp; <additional formals> is
   --    begin
   --       if not PrimSP (Obj) then
   --          $raise_assert_failure ("failed precondition in call at ...");
   --       end if;
   --
   --       Prim (Obj, ...);
   --    end Prim;

   procedure Merge_Class_Conditions (Spec_Id : Entity_Id);
   --  Merge and preanalyze all class-wide conditions of Spec_Id (class-wide
   --  preconditions merged with operator or-else; class-wide postconditions
   --  merged with operator and-then). Ignored pre/postconditions are also
   --  merged since, although they are not required to generate code, their
   --  preanalysis is required to perform semantic checks. Resulting merged
   --  expressions are later installed by the expander in helper subprograms
   --  which are invoked from the caller side; they are also used to build
   --  the dispatch-table wrapper (DTW), if required.

   procedure Preanalyze_Class_Conditions (Spec_Id : Entity_Id);
   --  Preanalyze class-wide pre-/postconditions of the given subprogram
   --  specification.

   procedure Process_Class_Conditions_At_Freeze_Point (Typ : Entity_Id);
   --  Merge, preanalyze, and check class-wide pre/postconditions of Typ
   --  primitives.

   procedure Save_Global_References_In_Contract
     (Templ  : Node_Id;
      Gen_Id : Entity_Id);
   --  Save all global references found within the aspect specifications and
   --  the contract-related source pragmas assocated with generic template
   --  Templ. Gen_Id denotes the entity of the analyzed generic copy.

end Contracts;
