------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ A G G R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2022, Free Software Foundation, Inc.         --
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

with Types; use Types;

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

   function Is_Null_Aggregate (N : Node_Id) return Boolean;
   --  Returns True for a "[]" aggregate (an Ada 2022 feature), even after
   --  it has been transformed by expansion. Returns False otherwise.

   function Is_Null_Array_Aggregate_High_Bound (N : Node_Id) return Boolean;
   --  Returns True for the high bound of a null array aggregate.

end Sem_Aggr;
