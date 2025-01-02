------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ A G G R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This package contains the resolution code for aggregates. It is logically
--  part of Sem_Res, but is split off since the aggregate code is so complex.

with Einfo.Entities; use Einfo.Entities;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Types;          use Types;

package Sem_Aggr is

   procedure Resolve_Delta_Aggregate     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Aggregate           (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Extension_Aggregate (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Container_Aggregate (N : Node_Id; Typ : Entity_Id);

   function Is_Others_Aggregate (Aggr : Node_Id) return Boolean;
   --  Returns True is aggregate Aggr consists of a single OTHERS choice

   function Is_Single_Aggregate (Aggr : Node_Id) return Boolean;
   --  Returns True if aggregate Aggr consists of a single choice

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function Is_Indexed_Aggregate
     (N           : N_Aggregate_Id;
      Add_Unnamed : Node_Id;
      New_Indexed : Node_Id) return Boolean;
   --  Returns True if N satisfies the criteria for being an indexed aggregate,
   --  that is, N is a container aggregate whose type has an Aggregate aspect
   --  that specifies a New_Indexed operation (it's Present), the aggregate
   --  is not a null aggregate, and either the type doesn't specify Add_Unnamed
   --  or there is a component association that is an N_Component_Association
   --  or is an N_Iterated_Component_Association with a Defining_Identifier.
   --  Returns False otherwise. The actuals for the Add_Unnamed and New_Indexed
   --  formals must be nodes that are names denoting the subprograms specified
   --  for those operations in the Aggregate aspect of the aggregate's type,
   --  or else Empty if the operation was not specified.

   function Is_Null_Aggregate (N : Node_Id) return Boolean;
   --  Returns True for a "[]" aggregate (an Ada 2022 feature), even after
   --  it has been transformed by expansion. Returns False otherwise.

   function Is_Null_Array_Aggregate_High_Bound (N : Node_Id) return Boolean;
   --  Returns True for the high bound of a null array aggregate.

   function Is_Deep_Choice
     (Choice    : Node_Id;
      Aggr_Type : Type_Kind_Id) return Boolean;
   --  Returns whether Choice from a delta aggregate of type Aggr_Type is a
   --  deep choice.

   function Is_Root_Prefix_Of_Deep_Choice (Pref : Node_Id) return Boolean;
   --  Returns whether prefix Pref of a deep choice is its root prefix. Except
   --  for its use in Is_Deep_Choice, this function should only be called on
   --  prefixes of a deep choice as identified by Is_Deep_Choice.

end Sem_Aggr;
