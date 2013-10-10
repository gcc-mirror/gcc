------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C A S E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2013, Free Software Foundation, Inc.         --
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

--  Package containing the routines to process a list of discrete choices.
--  Such lists can occur in two different constructs: case statements and
--  record variants. We have factorized what used to be two very similar
--  sets of routines in one place. These are not currently used for the
--  aggregate case, since issues with nested aggregates make that case
--  substantially different.

with Types; use Types;

package Sem_Case is

   procedure No_OP (C : Node_Id);
   --  The no-operation routine. Does absolutely nothing. Can be used
   --  in the following generic for the parameter Process_Empty_Choice.

   generic
      with function Get_Alternatives (N : Node_Id) return List_Id;
      --  Function used to get the list of case statement alternatives or
      --  record variants, from which we can then access the actual lists of
      --  discrete choices. N is the node for the original construct (case
      --  statement or a record variant).

      with procedure Process_Empty_Choice (Choice : Node_Id);
      --  Processing to carry out for an empty Choice. Set to No_Op (declared
      --  above) if no such processing is required.

      with procedure Process_Non_Static_Choice (Choice : Node_Id);
      --  Processing to carry out for a non static Choice

      with procedure Process_Associated_Node (A : Node_Id);
      --  Associated with each case alternative or record variant A there is
      --  a node or list of nodes that need semantic processing. This routine
      --  implements that processing.

   package Generic_Choices_Processing is

      procedure Analyze_Choices
        (N              : Node_Id;
         Subtyp         : Entity_Id;
         Raises_CE      : out Boolean;
         Others_Present : out Boolean);
      --  From a case expression, case statement, or record variant N, this
      --  routine analyzes the corresponding list of discrete choices. Subtyp
      --  is the subtype of the discrete choices. The type against which the
      --  discrete choices must be resolved is its base type.
      --
      --  If one of the bounds of a discrete choice raises a constraint
      --  error the flag Raise_CE is set.
      --
      --  Finally Others_Present is set to True if an Others choice is present
      --  in the list of choices, and in this case the call also sets
      --  Others_Discrete_Choices in the N_Others_Choice node.

   end Generic_Choices_Processing;

end Sem_Case;
