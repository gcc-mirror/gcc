------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D I S T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
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

--  This package contains utility routines used for the generation of the
--  stubs relevant to the distribution annex.

with Types; use Types;

package Exp_Dist is

   procedure Add_RAST_Features (Vis_Decl : in Node_Id);
   --  Build and add bodies for dereference and 'Access subprograms for a
   --  remote access to subprogram type. Vis_Decl is the declaration node for
   --  the RAS type.

   procedure Add_RACW_Features (RACW_Type : in Entity_Id);
   --  Add RACW features. If the RACW and the designated type are not in the
   --  same scope, then Add_RACW_Primitive_Declarations_And_Bodies is called
   --  automatically since we do know the primitive list already.

   procedure Add_RACW_Primitive_Declarations_And_Bodies
     (Designated_Type : in Entity_Id;
      Insertion_Node  : in Node_Id;
      Decls           : in List_Id);
   --  Add primitive for the stub type, and the RPC receiver. The declarations
   --  are inserted after insertion_Node, while the bodies are appened at the
   --  end of Decls.

   procedure Remote_Types_Tagged_Full_View_Encountered
     (Full_View : in Entity_Id);
   --  When a full view with a private view is encountered in a Remote_Types
   --  package and corresponds to a tagged type, then this procedure is called
   --  to generate the needed RACW features if it is needed.

   procedure RACW_Type_Is_Asynchronous (RACW_Type : in Entity_Id);
   --  This subprogram must be called when it is detected that the RACW type
   --  is asynchronous.

   procedure Expand_Calling_Stubs_Bodies (Unit_Node : in Node_Id);
   --  Call the expansion phase for the calling stubs. The code will be added
   --  at the end of the compilation unit, which is a package spec.

   procedure Expand_Receiving_Stubs_Bodies (Unit_Node : in Node_Id);
   --  Call the expansion phase for the calling stubs. The code will be added
   --  at the end of the compilation unit, which may be either a package spec
   --  or a package body.

   procedure Expand_All_Calls_Remote_Subprogram_Call (N : in Node_Id);
   --  Rewrite a call to a subprogram located in a Remote_Call_Interface
   --  package on which the pragma All_Calls_Remote applies so that it
   --  goes through the PCS. N is either an N_Procedure_Call_Statement
   --  or an N_Function_Call node.

   procedure Build_Passive_Partition_Stub (U : Node_Id);
   --  Build stub for a shared passive package. U is the analyzed
   --  compilation unit for a package declaration.

end Exp_Dist;
