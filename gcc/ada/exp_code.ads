------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C O D E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2019, Free Software Foundation, Inc.         --
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

--  Processing for handling code statements

with Types; use Types;

with System; use System;
package Exp_Code is

   procedure Expand_Asm_Call (N : Node_Id);
   --  Expands a call to Asm into an equivalent N_Code_Statement node

   --  The following routines provide an abstract interface to analyze
   --  code statements, for use by Gigi processing for code statements.
   --  Note that the implementations of these routines must not attempt
   --  to expand tables that are frozen on entry to Gigi.

   --  WARNING: There is a matching C declaration of these subprograms in fe.h

   function Is_Asm_Volatile (N : Node_Id) return Boolean;
   --  Given an N_Code_Statement node N, return True if Volatile=True is
   --  specified, and False if Volatile=False is specified (or set by default).

   function Asm_Template (N : Node_Id) return Node_Id;
   --  Given an N_Code_Statement node N, returns string literal node for
   --  template in call

   procedure Clobber_Setup (N : Node_Id);
   --  Given an N_Code_Statement node N, setup to process the clobber list
   --  with subsequent calls to Clobber_Get_Next.

   function Clobber_Get_Next return System.Address;
   --  Can only be called after a previous call to Clobber_Setup. The
   --  returned value is a pointer to a null terminated (C format) string
   --  for the next register argument. Null_Address is returned when there
   --  are no more arguments.

   procedure Setup_Asm_Inputs (N : Node_Id);
   --  Given an N_Code_Statement node N, setup to read list of Asm_Input
   --  arguments. The protocol is to construct a loop as follows:
   --
   --    Setup_Asm_Inputs (N);
   --    while Present (Asm_Input_Value)
   --      body
   --      Next_Asm_Input;
   --    end loop;
   --
   --  where the loop body calls Asm_Input_Constraint or Asm_Input_Value to
   --  obtain the constraint string or input value expression from the current
   --  Asm_Input argument.

   function Asm_Input_Constraint return Node_Id;
   --  Called within a loop initialized by Setup_Asm_Inputs and controlled
   --  by Next_Asm_Input as described above. Returns a string literal node
   --  for the constraint component of the current Asm_Input_Parameter, or
   --  Empty if there are no more Asm_Input parameters.

   function Asm_Input_Value return Node_Id;
   --  Called within a loop initialized by Setup_Asm_Inputs and controlled
   --  by Next_Asm_Input as described above. Returns the expression node for
   --  the value component of the current Asm_Input parameter, or Empty if
   --  there are no more Asm_Input parameters, or Error if an error was
   --  previously detected in the input parameters (note that the backend
   --  need not worry about this case, since it won't be called if there
   --  were any such serious errors detected).

   procedure Next_Asm_Input;
   --  Step to next Asm_Input parameter. It is an error to call this procedure
   --  if there are no more available parameters (which is impossible if the
   --  call appears in a loop as in the above example).

   procedure Setup_Asm_Outputs (N : Node_Id);
   --  Given an N_Code_Statement node N, setup to read list of Asm_Output
   --  arguments. The protocol is to construct a loop as follows:
   --
   --    Setup_Asm_Outputs (N);
   --    while Present (Asm_Output_Variable)
   --      body
   --      Next_Asm_Output;
   --    end loop;
   --
   --  where the loop body calls Asm_Output_Constraint or Asm_Output_Variable
   --  to obtain the constraint string or output variable name from the current
   --  Asm_Output argument.

   function Asm_Output_Constraint return Node_Id;
   --  Called within a loop initialized by Setup_Asm_Outputs and controlled
   --  by Next_Asm_Output as described above. Returns a string literal node
   --  for the constraint component of the current Asm_Output_Parameter, or
   --  Empty if there are no more Asm_Output parameters.

   function Asm_Output_Variable return Node_Id;
   --  Called within a loop initialized by Setup_Asm_Outputs and controlled by
   --  Next_Asm_Output as described above. Returns the expression node for the
   --  output variable component of the current Asm_Output parameter, or Empty
   --  if there are no more Asm_Output parameters, or Error if an error was
   --  previously detected in the input parameters (note that the backend need
   --  not worry about this case, since it won't be called if there were any
   --  such serious errors detected).

   procedure Next_Asm_Output;
   --  Step to next Asm_Output parameter. It is an error to call this procedure
   --  if there are no more available parameters (which is impossible if the
   --  call appears in a loop as in the above example).

end Exp_Code;
