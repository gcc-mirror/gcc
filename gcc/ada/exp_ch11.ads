------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C H 1 1                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

--  Expand routines for chapter 11 constructs

with Types; use Types;

package Exp_Ch11 is
   procedure Expand_N_Exception_Declaration          (N : Node_Id);
   procedure Expand_N_Handled_Sequence_Of_Statements (N : Node_Id);
   procedure Expand_N_Raise_Constraint_Error         (N : Node_Id);
   procedure Expand_N_Raise_Program_Error            (N : Node_Id);
   procedure Expand_N_Raise_Statement                (N : Node_Id);
   procedure Expand_N_Raise_Storage_Error            (N : Node_Id);
   procedure Expand_N_Subprogram_Info                (N : Node_Id);

   --  Data structures for gathering information to build exception tables
   --  See runtime routine Ada.Exceptions for full details on the format and
   --  content of these tables.

   procedure Initialize;
   --  Initializes these data structures for a new main unit file

   procedure Expand_At_End_Handler (HSS : Node_Id; Block : Node_Id);
   --  Given a handled statement sequence, HSS, for which the At_End_Proc
   --  field is set, and which currently has no exception handlers, this
   --  procedure expands the special exception handler required.
   --  This procedure also create a new scope for the given Block, if
   --  Block is not Empty.

   procedure Expand_Exception_Handlers (HSS : Node_Id);
   --  This procedure expands exception handlers, and is called as part
   --  of the processing for Expand_N_Handled_Sequence_Of_Statements and
   --  is also called from Expand_At_End_Handler. N is the handled sequence
   --  of statements that has the exception handler(s) to be expanded. This
   --  is also called to expand the special exception handler built for
   --  accept bodies (see Exp_Ch9.Build_Accept_Body).

   procedure Generate_Unit_Exception_Table;
   --  Procedure called by main driver to generate unit exception table if
   --  zero cost exceptions are enabled. See System.Exceptions for details.

   function Is_Non_Ada_Error (E : Entity_Id) return Boolean;
   --  This function is provided for Gigi use. It returns True if operating on
   --  VMS, and the argument E is the entity for System.Aux_Dec.Non_Ada_Error.
   --  This is used to generate the special matching code for this exception.

   procedure Remove_Handler_Entries (N : Node_Id);
   --  This procedure is called when optimization circuits determine that
   --  an entire subtree can be removed. If the subtree contains handler
   --  entries in zero cost exception mode, then such removal can lead to
   --  dangling references to non-existent handlers in the handler table.
   --  This procedure removes such references.

   --------------------------------------
   -- Subprogram_Descriptor Generation --
   --------------------------------------

   --  Subprogram descriptors are required for all subprograms, including
   --  explicit subprograms defined in the program, subprograms that are
   --  imported via pragma Import, and also for the implicit elaboration
   --  subprograms used to elaborate package specs and bodies.

   procedure Generate_Subprogram_Descriptor_For_Package
     (N    : Node_Id;
      Spec : Entity_Id);
   --  This is used to create a descriptor for the implicit elaboration
   --  procedure for a package spec of body. The compiler only generates
   --  such descriptors if the package spec or body contains exception
   --  handlers (either explicitly in the case of a body, or from generic
   --  package instantiations). N is the node for the package body or
   --  spec, and Spec is the package body or package entity respectively.
   --  N must be a compilation unit, and the descriptor is placed at
   --  the end of the actions for the auxiliary compilation unit node.

   procedure Generate_Subprogram_Descriptor_For_Subprogram
     (N    : Node_Id;
      Spec : Entity_Id);
   --  This is used to create a desriptor for a subprogram, both those
   --  present in the source, and those implicitly generated by code
   --  expansion. N is the subprogram body node, and Spec is the entity
   --  for the subprogram. The descriptor is placed at the end of the
   --  Last exception handler, or, if there are no handlers, at the end
   --  of the statement sequence.

   procedure Generate_Subprogram_Descriptor_For_Imported_Subprogram
     (Spec  : Entity_Id;
      Slist : List_Id);
   --  This is used to create a descriptor for an imported subprogram.
   --  Such descriptors are needed for propagation of exceptions through
   --  such subprograms. The descriptor never references any handlers,
   --  and is appended to the given Slist.

end Exp_Ch11;
