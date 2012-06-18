------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ D I M                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2012, Free Software Foundation, Inc.         --
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

--  This package provides support for numerical systems with dimensions. A
--  "dimension" is a compile-time property of a numerical type which represents
--  a relation between various quantifiers such as length, velocity, etc.

--  Package System.Dim.Mks offers a ready-to-use system of SI base units. In
--  addition, the implementation of this feature offers the ability to define
--  an arbitrary system of units through the use of Ada 2012 aspects.

--  Dimensionality checking is part of type analysis performed by the compiler.
--  It ensures that manipulation of quantified numeric values is sensible with
--  respect to the system of units.

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

with Types; use Types;

package Sem_Dim is

   procedure Analyze_Aspect_Dimension
     (N    : Node_Id;
      Id   : Entity_Id;
      Aggr : Node_Id);
   --  Analyze the contents of aspect Dimension. Associate the provided values
   --  and quantifiers with the related context N. Id is the corresponding
   --  Aspect_Id (Aspect_Dimension) Aggr is the corresponding expression for
   --  the aspect Dimension declared by the declaration of N.

   procedure Analyze_Aspect_Dimension_System
     (N    : Node_Id;
      Id   : Entity_Id;
      Aggr : Node_Id);
   --  Analyze the contents of aspect Dimension_System. Extract the numerical
   --  type, unit name and corresponding symbol from each indivitual dimension.
   --  Id is the corresponding Aspect_Id (Aspect_Dimension_System)
   --  Aggr is the corresponding expression for the aspect Dimension_System
   --  declared by the declaration of N.

   procedure Analyze_Dimension (N : Node_Id);
   --  N may denote any of the following contexts:
   --    * assignment statement
   --    * attribute reference
   --    * binary operator
   --    * compontent declaration
   --    * extended return statement
   --    * function call
   --    * identifier
   --    * indexed component
   --    * object declaration
   --    * object renaming declaration
   --    * qualified expression
   --    * selected component
   --    * simple return statement
   --    * slice
   --    * subtype declaration
   --    * type conversion
   --    * unary operator
   --    * unchecked type conversion
   --  Depending on the context, ensure that all expressions and entities
   --  involved do not violate the rules of a system.

   procedure Eval_Op_Expon_For_Dimensioned_Type
     (N    : Node_Id;
      Btyp : Entity_Id);
   --  Evaluate the Expon operator for dimensioned type with rational exponent.
   --  Indeed the regular Eval_Op_Expon routine (see package Sem_Eval) is
   --  restricted to Integer exponent. This routine deals only with rational
   --  exponent which is not an integer if Btyp is a dimensioned type.

   procedure Expand_Put_Call_With_Symbol (N : Node_Id);
   --  Determine whether N denotes a subprogram call to one of the routines
   --  defined in System.Dim.Float_IO or System.Dim.Integer_IO and add an
   --  extra actual to the call to represent the symbolic representation of
   --  a dimension.

   function Has_Dimension_System (Typ : Entity_Id) return Boolean;
   --  Return True if type Typ has aspect Dimension_System applied to it

   function Is_Dim_IO_Package_Instantiation (N : Node_Id) return Boolean;
   --  Return True if N is a package instantiation of System.Dim.Integer_IO or
   --  of System.Dim.Float_IO.

   procedure Remove_Dimension_In_Call (Call : Node_Id);
   --  Remove the dimensions from all formal parameters of Call

   procedure Remove_Dimension_In_Statement (Stmt : Node_Id);
   --  Remove the dimensions associated with Stmt

end Sem_Dim;
