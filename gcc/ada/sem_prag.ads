------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ P R A G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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

--  Pragma handling is isolated in a separate package
--  (logically this processing belongs in chapter 4)

with Types; use Types;
package Sem_Prag is

   procedure Analyze_Pragma (N : Node_Id);
   --  Analyze procedure for pragma reference node N

   function Is_Pragma_String_Literal (Par : Node_Id) return Boolean;
   --  Given an N_Pragma_Argument_Association node, Par, which has the form
   --  of an operator symbol, determines whether or not it should be treated
   --  as an string literal. This is called by Sem_Ch6.Analyze_Operator_Symbol.
   --  If True is returned, the argument is converted to a string literal. If
   --  False is returned, then the argument is treated as an entity reference
   --  to the operator.

   procedure Process_Compilation_Unit_Pragmas (N : Node_Id);
   --  Called at the start of processing compilation unit N to deal with
   --  any special issues regarding pragmas. In particular, we have to
   --  deal with Suppress_All at this stage, since it appears after the
   --  unit instead of before.

   procedure Set_Encoded_Interface_Name (E : Entity_Id; S : Node_Id);
   --  This routine is used to set an encoded interface name. The node
   --  S is an N_String_Literal node for the external name to be set, and
   --  E is an entity whose Interface_Name field is to be set. In the
   --  normal case where S contains a name that is a valid C identifier,
   --  then S is simply set as the value of the Interface_Name. Otherwise
   --  it is encoded. See the body for details of the encoding. This
   --  encoding is only done on VMS systems, since it seems pretty silly,
   --  but is needed to pass some dubious tests in the test suite.

end Sem_Prag;
