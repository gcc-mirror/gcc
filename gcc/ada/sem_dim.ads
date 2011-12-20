------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ D I M                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2011, Free Software Foundation, Inc.            --
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

--  This new package of the GNAT compiler has been created in order to enable
--  any user of the GNAT compiler to deal with physical issues.

--  Indeed, the user is now able to create their own dimension system and to
--  assign a dimension, defined from the MKS system (package System.Dim_Mks)
--  or their own dimension systems, with any item and to run operations with
--  dimensionned entities.

--  In that case, a dimensionality checking will be performed at compile time.
--  If no dimension has been assigned, the compiler assumes that the item is
--  dimensionless.

-----------------------------
-- Aspect_Dimension_System --
-----------------------------

--  In order to enable the user to create their own dimension system, a new
--  aspect: Aspect_Dimension_System has been created.

--  Note that this aspect applies for type declaration of type derived from any
--  numeric type.

--  It defines the names of each dimension

----------------------
-- Aspect_Dimension --
----------------------

--  This new aspect applies for subtype and object declarations in order to
--  define new dimensions.

--  Using this aspect, the user is able to create new subtype/object with any
--  dimension needed.

--  Note that the base type of the subtype/object must be the type that defines
--  the corresponding dimension system.

--  The expression of this aspect is an aggregate of rational values for each
--  dimension in the corresponding dimension system.

-------------------------------------------
-- Dimensionality checking & propagation --
-------------------------------------------

--  For each node (when needed), a dimension analysis (Analyze_Dimension) is
--  performed as part of the Resolution routine or the Analysis routine if no
--  Resolution.

--  The dimension analysis is divided into two phases:

--  Phase 1: dimension checking

--  Phase 2: propagation of dimensions

--  Depending on the node kind, either none, one phase or two phases are
--  executed.

--  Phase 2 is called only when the node allows a dimension (see body of
--  Sem_Dim to get the list of nodes that permit dimensions).

------------------
-- Dimension_IO --
------------------

--  This section contains the routine used for IO purposes

with Types; use Types;

package Sem_Dim is

   -----------------------------
   -- Aspect_Dimension_System --
   -----------------------------

   procedure Analyze_Aspect_Dimension_System
     (N    : Node_Id;
      Id   : Node_Id;
      Expr : Node_Id);
   --  Analyzes the aggregate of Aspect_Dimension_System

   ----------------------
   -- Aspect_Dimension --
   ----------------------

   procedure Analyze_Aspect_Dimension
     (N    : Node_Id;
      Id   : Node_Id;
      Expr : Node_Id);
   --  Analyzes the aggregate of Aspect_Dimension and attaches the
   --  corresponding dimension to N.

   -------------------------------------------
   -- Dimensionality checking & propagation --
   -------------------------------------------

   procedure Analyze_Dimension (N : Node_Id);
   --  Performs a dimension analysis and propagates dimension between nodes
   --  when needed.

   procedure Eval_Op_Expon_For_Dimensioned_Type
     (N     : Node_Id;
      B_Typ : Entity_Id);
   --  Evaluate the Expon operator for dimensioned type with rational exponent

   function Is_Dimensioned_Type (E : Entity_Id) return Boolean;
   --  Return True if the type is a dimensioned type (i.e: a type which has an
   --  aspect Dimension_System)

   procedure Remove_Dimension_In_Call (N : Node_Id);
   --  At the end of the Expand_Call routine, remove the dimensions of every
   --  parameter in the call N.

   procedure Remove_Dimension_In_Declaration (D : Node_Id);
   --  At the end of Analyze_Declarations routine (see Sem_Ch3), removes the
   --  dimension of the expression for each declaration.

   procedure Remove_Dimension_In_Statement (S : Node_Id);
   --  At the end of the Analyze_Statements routine (see Sem_Ch5), removes the
   --  dimension for every statements.

   ------------------
   -- Dimension_IO --
   ------------------

   procedure Expand_Put_Call_With_Dimension_String (N : Node_Id);
   --  Expansion of Put call (from package System.Dim_Float_IO and
   --  System.Dim_Integer_IO) for a dimensioned object in order to add the
   --  dimension symbols as a suffix of the numeric value.

end Sem_Dim;
