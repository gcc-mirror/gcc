------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 8                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Types; use Types;
package Sem_Ch8 is

   -----------------------------------
   -- Handling extensions of System --
   -----------------------------------

   --  For targets that define a much larger System package than given in
   --  the RM, we use a child package containing additional declarations,
   --  which is loaded when needed, and whose entities are conceptually
   --  within System itself. The presence of this auxiliary package is
   --  controlled with the pragma Extend_System. The following variable
   --  holds the entity of the auxiliary package, to simplify the special
   --  visibility rules that apply to it.

   System_Aux_Id : Entity_Id := Empty;

   -----------------
   -- Subprograms --
   -----------------

   procedure Analyze_Exception_Renaming                 (N : Node_Id);
   procedure Analyze_Expanded_Name                      (N : Node_Id);
   procedure Analyze_Generic_Function_Renaming          (N : Node_Id);
   procedure Analyze_Generic_Package_Renaming           (N : Node_Id);
   procedure Analyze_Generic_Procedure_Renaming         (N : Node_Id);
   procedure Analyze_Object_Renaming                    (N : Node_Id);
   procedure Analyze_Package_Renaming                   (N : Node_Id);
   procedure Analyze_Subprogram_Renaming                (N : Node_Id);
   procedure Analyze_Use_Package                        (N : Node_Id);
   procedure Analyze_Use_Type                           (N : Node_Id);

   procedure End_Scope;
   --  Called at end of scope. On exit from blocks and bodies (subprogram,
   --  package, task, and protected bodies), the name of the current scope
   --  must be removed from the scope stack, and the local entities must be
   --  removed from their homonym chains. On exit from record declarations,
   --  from package specifications, and from tasks and protected type
   --  specifications, more specialized procedures are invoked.

   procedure End_Use_Clauses (Clause : Node_Id);
   --  Invoked on scope exit, to undo the effect of local use clauses. U is
   --  the first Use clause of a scope being exited. This can be the current
   --  scope, or some enclosing scopes when building a clean environment to
   --  compile an instance body for inlining.

   procedure End_Use_Package (N : Node_Id);
   procedure End_Use_Type    (N : Node_Id);
   --  Subsidiaries of End_Use_Clauses.  Also called directly for use clauses
   --  appearing in context clauses.

   procedure Find_Direct_Name (N : Node_Id);
   --  Given a direct name (Identifier or Operator_Symbol), this routine
   --  scans the homonym chain for the name searching for corresponding
   --  visible entities to find the referenced entity (or in the case of
   --  overloading), entities. On return, the Entity, and Etype fields
   --  are set. In the non-overloaded case, these are the correct final
   --  entries. In the overloaded case, Is_Overloaded is set, Etype and
   --  Entity refer to an arbitrary element of the overloads set, and
   --  an appropriate list of entries has been made in the overload
   --  interpretation table (to be disambiguated in the resolve phase).

   procedure Find_Selected_Component (N : Node_Id);
   --  Resolve various cases of selected components, recognize expanded names

   procedure Find_Type (N : Node_Id);
   --  Perform name resolution, and verify that the name found is that of a
   --  type. On return the Entity and Etype fields of the node N are set
   --  appropriately. If it is an incomplete type whose full declaration has
   --  been seen, they are set to the entity in the full declaration.
   --  Similarly, if the type is private, it has received a full declaration,
   --  and we are in the private part or body of the package, then the two
   --  fields are set to the entity of the full declaration as well. This
   --  procedure also provides special processing for Class types as well.

   function Get_Full_View (T_Name : Entity_Id) return Entity_Id;
   --  If T_Name is an incomplete type and the full declaration has been
   --  seen, or is the name of a class_wide type whose root is incomplete.
   --  return the corresponding full declaration.

   procedure Initialize;
   --  Initializes data structures used for visibility analysis. Must be
   --  called before analyzing each new main source program.

   procedure Install_Use_Clauses (Clause : Node_Id);
   --  applies the use clauses appearing in a given declarative part,
   --  when the corresponding scope has been placed back on the scope
   --  stack after unstacking to compile a different context (subunit or
   --  parent of generic body).

   function In_Open_Scopes (S : Entity_Id) return Boolean;
   --  S is the entity of a scope. This function determines if this scope
   --  is currently open (i.e. it appears somewhere in the scope stack).

   procedure New_Scope (S : Entity_Id);
   --  Make new scope stack entry, pushing S, the entity for a scope
   --  onto the top of the scope table. The current setting of the scope
   --  suppress flags is saved for restoration on exit.

   procedure Pop_Scope;
   --  Remove top entry from scope stack, restoring the saved setting
   --  of the scope suppress flags.

   function Present_System_Aux (N : Node_Id := Empty) return Boolean;
   --  Return True if the auxiliary system file has been successfully loaded.
   --  Otherwise attempt to load it, using the name supplied by a previous
   --  Extend_System pragma, and report on the success of the load.
   --  If N is present, it is a selected component whose prefix is System,
   --  or else a with-clause on system. N is absent when the function is
   --  called to find the visibility of implicit operators.

   procedure Restore_Scope_Stack (Handle_Use : Boolean := True);
   procedure Save_Scope_Stack (Handle_Use : Boolean := True);
   --  These two procedures are called from Semantics, when a unit U1 is
   --  to be compiled in the course of the compilation of another unit U2.
   --  This happens whenever Rtsfind is called. U1, the unit retrieved by
   --  Rtsfind, must be compiled in its own context, and the current scope
   --  stack containing U2 and local scopes must be made unreachable. On
   --  return, the contents of the scope stack must be made accessible again.
   --  The flag Handle_Use indicates whether local use clauses must be
   --  removed/installed. In the case of inlining of instance bodies, the
   --  visiblity handling is done fully in Inline_Instance_Body, and use
   --  clauses are handled there.

   procedure Set_Use (L : List_Id);
   --  Find use clauses that are declarative items in a package declaration
   --  and  set the potentially use-visible flags of imported entities before
   --  analyzing the corresponding package body.

end Sem_Ch8;
