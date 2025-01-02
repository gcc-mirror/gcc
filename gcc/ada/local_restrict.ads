------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        L O C A L _ R E S T R I C T                       --
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

--  This package deals with the implementation of the Local_Restrictions aspect

with Types;  use Types;

package Local_Restrict is
   type Local_Restriction is (No_Secondary_Stack, No_Heap_Allocations);
   --  A local restriction can be mentioned in a Local_Restrictions aspect
   --  specification. A local restriction might apply, for example, to a
   --  single subprogram. No_Secondary_Stack corresponds to the
   --  GNAT-defined restriction of the same name. No_Heap_Allocations
   --  corresponds to the conjunction of the RM-defined restrictions
   --  No_Allocators and No_Implicit_Heap_Allocations. If a subprogram is
   --  subject to a local restriction, then any subprogram that it calls
   --  shall be known to satisfy that restriction.

   type Local_Restriction_Set is array (Local_Restriction) of Boolean;

   procedure Check_Call (Call : Node_Id; Callee : Entity_Id := Empty);
   --  Check whether a call violates any local restrictions that are
   --  in effect. An empty callee indicates that the callee should be
   --  conservatively assumed to violate any local restrictions that
   --  are in effect (for example, for an entry call or a call through
   --  an access-to-subprogram value).

   procedure Check_Overriding (Overrider_Op, Overridden_Op : Entity_Id);
   --  Check that all of the local restrictions in effect for
   --  Overridden_Op are also in effect for Overrider_Op.

   procedure Check_Actual_Subprogram_For_Instance
     (Actual_Subp_Name : Node_Id; Formal_Subp : Entity_Id);
   --  Check that all of the local restrictions in effect for
   --  Formal_Subp are also in effect for Actual_Subp.

end Local_Restrict;
