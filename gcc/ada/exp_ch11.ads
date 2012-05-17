------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C H 1 1                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

   function Find_Local_Handler
     (Ename : Entity_Id;
      Nod   : Node_Id) return Node_Id;
   --  This function searches for a local exception handler that will handle
   --  the exception named by Ename. If such a local hander exists, then the
   --  corresponding N_Exception_Handler is returned. If no such handler is
   --  found then Empty is returned. In order to match and return True, the
   --  handler may not have a choice parameter specification. Nod is the raise
   --  node that references the handler.

   function Get_Local_Raise_Call_Entity return Entity_Id;
   --  This function is provided for use by the back end in conjunction with
   --  generation of Local_Raise calls when an exception raise is converted to
   --  a goto statement. If Local_Raise is defined, its entity is returned,
   --  if not, Empty is returned (in which case the call is silently skipped).

   function Get_RT_Exception_Entity (R : RT_Exception_Code) return Entity_Id;
   --  This function is provided for use by the back end in conjunction with
   --  generation of Local_Raise calls when an exception raise is converted to
   --  a goto statement. The argument is the reason code which would be used
   --  to determine which Rcheck_nn procedure to call. The returned result is
   --  the exception entity to be passed to Local_Raise.

   procedure Get_RT_Exception_Name (Code : RT_Exception_Code);
   --  This procedure is provided for use by the back end to obtain the name of
   --  the Rcheck procedure for Code. The name is appended to Namet.Name_Buffer
   --  without the __gnat_rcheck_ prefix.

   function Is_Non_Ada_Error (E : Entity_Id) return Boolean;
   --  This function is provided for Gigi use. It returns True if operating on
   --  VMS, and the argument E is the entity for System.Aux_Dec.Non_Ada_Error.
   --  This is used to generate the special matching code for this exception.

   procedure Possible_Local_Raise (N : Node_Id; E : Entity_Id);
   --  This procedure is called whenever node N might cause the back end
   --  to generate a local raise for a local Constraint/Program/Storage_Error
   --  exception. It deals with generating a warning if there is no local
   --  handler (and restriction No_Exception_Propagation is set), or if there
   --  is a local handler marking that it has a local raise. E is the entity
   --  of the corresponding exception.
end Exp_Ch11;
