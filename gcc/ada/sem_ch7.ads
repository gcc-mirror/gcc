------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 7                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

with Types; use Types;

package Sem_Ch7 is

   procedure Analyze_Package_Body                       (N : Node_Id);
   procedure Analyze_Package_Declaration                (N : Node_Id);
   procedure Analyze_Package_Specification              (N : Node_Id);
   procedure Analyze_Private_Type_Declaration           (N : Node_Id);

   procedure End_Package_Scope (P : Entity_Id);
   --  Calls Uninstall_Declarations, and then pops the scope stack

   procedure Exchange_Declarations (Id : Entity_Id);
   --  Exchange private and full declaration on entry/exit from a package
   --  declaration or body. The semantic links of the respective nodes
   --  are preserved in the exchange.

   procedure Install_Visible_Declarations (P : Entity_Id);
   procedure Install_Private_Declarations (P : Entity_Id);

   --  On entrance to a package body, make declarations in package spec
   --  immediately visible.
   --
   --  When compiling the body of a package,  both routines are called in
   --  succession. When compiling the body of a child package, the call
   --  to Install_Private_Declaration is immediate for private children,
   --  but is deferred until the compilation of the  private part of the
   --  child for public child packages.

   function Unit_Requires_Body
     (Pack_Id            : Entity_Id;
      Do_Abstract_States : Boolean := False) return Boolean;
   --  Determine whether package Pack_Id requires a body. A specification needs
   --  a body if it contains declarations that require completion in the body.
   --  A non-Ghost [generic] package does not require a body when it declares
   --  Ghost entities exclusively. When flag Do_Abstract_States is set to True,
   --  non-null abstract states are considered in determining the need for a
   --  body.

   procedure May_Need_Implicit_Body (E : Entity_Id);
   --  If a package declaration contains tasks or RACWs and does not require
   --  a body, create an implicit body at the end of the current declarative
   --  part to activate those tasks or contain the bodies for the RACW
   --  calling stubs.

   procedure New_Private_Type (N : Node_Id; Id : Entity_Id; Def : Node_Id);
   --  Common processing for private type declarations and for formal private
   --  type declarations. For private types, N and Def are the type declaration
   --  node; for formal private types, Def is the formal type definition.

   procedure Uninstall_Declarations (P : Entity_Id);
   --  At the end of a package declaration or body, declarations in the visible
   --  part are no longer immediately visible, and declarations in the private
   --  part are not visible at all. For inner packages, place visible entities
   --  at the end of their homonym chains. For compilation units, make
   --  all entities invisible. In both cases, exchange private and visible
   --  declarations to restore order of elaboration.

end Sem_Ch7;
