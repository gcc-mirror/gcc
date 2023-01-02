------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ S M E M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1998-2023, Free Software Foundation, Inc.         --
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

--  This package contains routines involved in the required expansions for
--  handling shared memory accesses for variables in Shared_Passive packages.

--  See detailed documentation in System.Shared_Storage spec for a full
--  description of the approach that is taken for handling distributed
--  shared memory. This expansion unit in the compiler is responsible
--  for generating the calls to routines in System.Shared_Storage.

with Types; use Types;
package Exp_Smem is

   procedure Expand_Shared_Passive_Variable (N : Node_Id);
   --  N is the identifier for a shared passive variable. This routine is
   --  responsible for determining if this is an assigned to N, or a
   --  reference to N, and generating the required calls to the shared
   --  memory read/write procedures.

   procedure Add_Shared_Var_Lock_Procs (N : Node_Id);
   --  The argument is a protected subprogram call, before it is rewritten
   --  by Exp_Ch9.Build_Protected_Subprogram_Call. This routine, which is
   --  called only in the case of an external call to a protected object
   --  that has Is_Shared_Passive set, deals with installing a transient scope
   --  and acquiring the appropriate global lock calls for this case. It also
   --  generates the necessary read/write calls for the protected object within
   --  the lock region.

   function Make_Shared_Var_Procs (N : Node_Id) return Node_Id;
   --  N is the node for the declaration of a shared passive variable.
   --  This procedure constructs an instantiation of
   --  System.Shared_Storage.Shared_Var_Procs that contains the read and
   --  assignment procedures for the shared memory variable.
   --  See System.Shared_Storage for a full description of these procedures
   --  and how they are used. The last inserted node is returned.

end Exp_Smem;
