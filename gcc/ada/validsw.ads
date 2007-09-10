------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              V A L I D S W                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2007, Free Software Foundation, Inc.         --
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

--  This unit contains the routines used to handle setting of validity
--  checking options.

package Validsw is

   -----------------------------
   -- Validity Check Switches --
   -----------------------------

   --  The following flags determine the specific set of validity checks
   --  to be made if validity checking is active (Validity_Checks_On = True)

   --  See GNAT users guide for an exact description of each option. The letter
   --  given in the comment is the letter used in the -gnatV compiler switch
   --  or in the argument of a Validity_Checks pragma to activate the option.
   --  The corresponding upper case letter deactivates the option.

   Validity_Check_Copies : Boolean := False;
   --  Controls the validity checking of copies. If this switch is set to
   --  true using -gnatVc, or a 'c' in the argument of a Validity_Checks
   --  pragma, then the right side of assignments and also initializing
   --  expressions in object declarations are checked for validity.

   Validity_Check_Components : Boolean := False;
   --  Controls validity checking for assignment to elementary components of
   --  records. If this switch is set true using -gnatVe, or an 'e' in the
   --  argument of Validity_Checks pragma, then the right hand of an assignment
   --  to such a component is checked for validity.

   Validity_Check_Default : Boolean := True;
   --  Controls default (reference manual) validity checking. If this switch is
   --  set to True using -gnatVd or a 'd' in the argument of a Validity_ Checks
   --  pragma (or the initial default value is used, set True), then left side
   --  subscripts and case statement arguments are checked for validity. This
   --  switch is also set by default if no -gnatV switch is used and no
   --  Validity_Checks pragma is processed.

   Validity_Check_Floating_Point : Boolean := False;
   --  Normally validity checking applies only to discrete values (integer
   --  and enumeration types). If this switch is set to True using -gnatVf
   --  or an 'f' in the argument of a Validity_Checks pragma, then floating-
   --  point values are also checked. The context in which such checks
   --  occur depends on other flags, e.g. if Validity_Check_Copies is also
   --  set then floating-point values on the right side of an assignment
   --  will be validity checked.

   Validity_Check_In_Out_Params : Boolean := False;
   --  Controls the validity checking of IN OUT parameters. If this switch
   --  is set to True using -gnatVm or a 'm' in the argument of a pragma
   --  Validity_Checks, then the initial value of all IN OUT parameters
   --  will be checked at the point of call of a procecure. Note that the
   --  character 'm' here stands for modified (parameters).

   Validity_Check_In_Params : Boolean := False;
   --  Controls the validity checking of IN parameters. If this switch is
   --  set to True using -gnatVm or an 'i' in the argument of a pragma
   --  Validity_Checks, then the initial value of all IN parameters
   --  will be checked at the point of call of a procecure or function.

   Validity_Check_Operands : Boolean := False;
   --  Controls validity checking of operands. If this switch is set to
   --  True using -gnatVo or an 'o' in the argument of a Validity_Checks
   --  pragma, then operands of all predefined operators and attributes
   --  will be validity checked.

   Validity_Check_Parameters : Boolean := False;
   --  This controls validity treatment for parameters within a subprogram.
   --  Normally if validity checking is enabled for parameters on a call
   --  (Validity_Check_In[_Out]_Params) then an assumption is made that the
   --  parameter values are valid on entry and not checked again within a
   --  procedure. Setting Validity_Check_Parameters removes this assumption
   --  and ensures that no assumptions are made about parameters, so that
   --  they will always be checked.

   Validity_Check_Returns : Boolean := False;
   --  Controls validity checking of returned values. If this switch is set
   --  to True using -gnatVr, or an 'r' in the argument of a Validity_Checks
   --  pragma, then the expression in a RETURN statement is validity checked.

   Validity_Check_Subscripts : Boolean := False;
   --  Controls validity checking of subscripts. If this switch is set to
   --  True using -gnatVs, or an 's' in the argument of a Validity_Checks
   --  pragma, then all subscripts are checked for validity. Note that left
   --  side subscript checking is controlled also by Validity_Check_Default.
   --  If Validity_Check_Subscripts is True, then all subscripts are checked,
   --  otherwise if Validity_Check_Default is True, then left side subscripts
   --  are checked, otherwise no subscripts are checked.

   Validity_Check_Tests : Boolean := False;
   --  Controls validity checking of tests that occur in conditions (i.e. the
   --  tests in IF, WHILE, and EXIT statements, and in entry guards). If this
   --  switch is set to True using -gnatVt, or a 't' in the argument of a
   --  Validity_Checks pragma, then all such conditions are validity checked.

   Force_Validity_Checks : Boolean := False;
   --  Normally, operands that do not come from source (i.e. cases of expander
   --  generated code) are not checked, if this flag is set True, then checking
   --  of such operands is forced (if Validity_Check_Operands is set).

   -----------------
   -- Subprograms --
   -----------------

   procedure Set_Default_Validity_Check_Options;
   --  This procedure is called to set the default validity checking options
   --  that apply if no Validity_Check switches or pragma is given.

   procedure Set_Validity_Check_Options
     (Options  : String;
      OK       : out Boolean;
      Err_Col  : out Natural);
   --  This procedure is called to set the validity check options that
   --  correspond to the characters in the given Options string. If
   --  all options are valid, then Set_Default_Validity_Check_Options
   --  is first called to set the defaults, and then the options in the
   --  given string are set in an additive manner. If any invalid character
   --  is found, then OK is False on exit, and Err_Col is the index in
   --  in options of the bad character. If all options are valid, then
   --  OK is True on return, and Err_Col is set to options'Last + 1.

   procedure Set_Validity_Check_Options (Options : String);
   --  Like the above procedure, except that the call is simply ignored if
   --  there are any error conditions, this is for example appopriate for
   --  calls where the string is known to be valid, e.g. because it was
   --  obtained by Save_Validity_Check_Options.

   procedure Reset_Validity_Check_Options;
   --  Sets all validity check options to off

   subtype Validity_Check_Options is String (1 .. 16);
   --  Long enough string to hold all options from Save call below

   procedure Save_Validity_Check_Options
     (Options : out Validity_Check_Options);
   --  Sets Options to represent current selection of options. This
   --  set can be restored by first calling Reset_Validity_Check_Options,
   --  and then calling Set_Validity_Check_Options with the Options string.

end Validsw;
