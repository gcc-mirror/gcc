------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               I N L I N E                                --
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

--  This module handles two kinds of inlining activity:

--  a) Instantiation of generic bodies. This is done unconditionally, after
--  analysis and expansion of the main unit.

--  b) Compilation of unit bodies that contain the bodies of inlined sub-
--  programs. This is done only if inlining is enabled (-gnatn). Full inlining
--  requires that a) an b) be mutually recursive, because each step may
--  generate another generic expansion and further inlined calls. For now each
--  of them uses a workpile algorithm, but they are called independently from
--  Frontend, and thus are not mutually recursive.

with Alloc;
with Table;
with Types;  use Types;

package Inline is

   --------------------------------
   -- Generic Body Instantiation --
   --------------------------------

   --  The bodies of generic instantiations are built after semantic analysis
   --  of the main unit is complete. Generic instantiations are saved in a
   --  global data structure, and the bodies constructed by means of a separate
   --  analysis and expansion step.

   --  See full description in body of Sem_Ch12 for details

   type Pending_Body_Info is record
      Inst_Node : Node_Id;
      --  Node for instantiation that requires the body

      Act_Decl : Node_Id;
      --  Declaration for package or subprogram spec for instantiation

      Expander_Status : Boolean;
      --  If the body is instantiated only for semantic checking, expansion
      --  must be inhibited.

      Current_Sem_Unit : Unit_Number_Type;
      --  The semantic unit within which the instantiation is found. Must
      --  be restored when compiling the body, to insure that internal enti-
      --  ties use the same counter and are unique over spec and body.
   end record;

   package Pending_Instantiations is new Table.Table (
     Table_Component_Type => Pending_Body_Info,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Pending_Instantiations_Initial,
     Table_Increment      => Alloc.Pending_Instantiations_Increment,
     Table_Name           => "Pending_Instantiations");

   --  The following table records subprograms and packages for which
   --  generation of subprogram descriptors must be delayed.

   package Pending_Descriptor is new Table.Table (
     Table_Component_Type => Entity_Id,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Pending_Instantiations_Initial,
     Table_Increment      => Alloc.Pending_Instantiations_Increment,
     Table_Name           => "Pending_Descriptor");

   Analyzing_Inlined_Bodies : Boolean;
   --  This flag is set False by the call to Initialize, and then is set
   --  True by the call to Analyze_Inlined_Bodies. It is used to suppress
   --  generation of subprogram descriptors for inlined bodies.

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize internal tables

   procedure Lock;
   --  Lock internal tables before calling backend

   procedure Instantiate_Bodies;
   --  This procedure is called after semantic analysis is complete, to
   --  instantiate the bodies of generic instantiations that appear in the
   --  compilation unit.

   procedure Add_Inlined_Body (E : Entity_Id);
   --  E is an inlined subprogram appearing in a call, either explicitly, or
   --  a discriminant check for which gigi builds a call.  Add E's enclosing
   --  unit to Inlined_Bodies so that body of E can be subsequently retrieved
   --  and analyzed.

   procedure Analyze_Inlined_Bodies;
   --  At end of compilation, analyze the bodies of all units that contain
   --  inlined subprograms that are actually called.

   procedure Check_Body_For_Inlining (N : Node_Id; P : Entity_Id);
   --  If front-end inlining is enabled and a package declaration contains
   --  inlined subprograms, load and compile the package body to collect the
   --  bodies of these subprograms, so they are available to inline calls.
   --  N is the compilation unit for the package.

   procedure Remove_Dead_Instance (N : Node_Id);
   --  If an instantiation appears in unreachable code, delete the pending
   --  body instance.

end Inline;
