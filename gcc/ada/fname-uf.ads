------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F N A M E . U F                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

--  This child package contains the routines to translate a unit name to
--  a file name taking into account Source_File_Name pragmas. It also
--  contains the auxiliary routines used to record data from the pragmas.

--  Note: the reason we split this into a child unit is that the routines
--  for unit name translation have a significant number of additional
--  dependencies, including osint, and hence sdefault. There are a number
--  of tools that use utility subprograms in the Fname parent, but do not
--  need the functionality in this child package (and certainly do not want
--  to deal with the extra dependencies).

with Casing; use Casing;

package Fname.UF is

   -----------------
   -- Subprograms --
   -----------------

   type Expected_Unit_Type is (Expect_Body, Expect_Spec, Unknown);
   --  Return value from Get_Expected_Unit_Type

   function Get_Expected_Unit_Type
     (Fname : File_Name_Type) return Expected_Unit_Type;
   --  If possible, determine whether the given file name corresponds to a unit
   --  that is a spec or body (e.g. by examining the extension). If this cannot
   --  be determined with the file naming conventions in use, then the returned
   --  value is set to Unknown.

   function Get_File_Name
     (Uname    : Unit_Name_Type;
      Subunit  : Boolean;
      May_Fail : Boolean := False) return File_Name_Type;
   --  This function returns the file name that corresponds to a given unit
   --  name, Uname. The Subunit parameter is set True for subunits, and
   --  false for all other kinds of units. The caller is responsible for
   --  ensuring that the unit name meets the requirements given in package
   --  Uname and described above.
   --
   --  When May_Fail is True, if the file cannot be found, this function
   --  returns No_File. When it is False, if the file cannot be found,
   --  a file name compatible with one pattern Source_File_Name pragma is
   --  returned.

   function Get_Unit_Index (Uname : Unit_Name_Type) return Nat;
   --  If there is a specific Source_File_Name pragma for this unit, then
   --  return the corresponding unit index value. Return 0 if no index given.

   procedure Initialize;
   --  Initialize internal tables. This is called automatically when the
   --  package body is elaborated, so an explicit call to Initialize is
   --  only required if it is necessary to reinitialize the source file
   --  name pragma tables.

   procedure Lock;
   --  Lock tables before calling back end

   function File_Name_Of_Spec (Name : Name_Id) return File_Name_Type;
   --  Returns the file name that corresponds to the spec of a given unit
   --  name. The unit name here is not encoded as a Unit_Name_Type, but is
   --  rather just a normal form name in lower case, e.g. "xyz.def".

   function File_Name_Of_Body (Name : Name_Id) return File_Name_Type;
   --  Returns the file name that corresponds to the body of a given unit
   --  name. The unit name here is not encoded as a Unit_Name_Type, but is
   --  rather just a normal form name in lower case, e.g. "xyz.def".

   procedure Set_File_Name
     (U     : Unit_Name_Type;
      F     : File_Name_Type;
      Index : Nat);
   --  Make association between given unit name, U, and the given file name,
   --  F. This is the routine called to process a Source_File_Name pragma.
   --  Index is the value from the index parameter of the pragma if present
   --  and zero if no index parameter is present.

   procedure Set_File_Name_Pattern
     (Pat : String_Ptr;
      Typ : Character;
      Dot : String_Ptr;
      Cas : Casing_Type);
   --  This is called to process a Source_File_Name pragma whose first
   --  argument is a file name pattern string.  Pat is this pattern string,
   --  which contains an asterisk to correspond to the unit. Typ is one of
   --  'b'/'s'/'u' for body/spec/subunit, Dot is the separator string
   --  for child/subunit names, and Cas is one of Lower/Upper/Mixed
   --  indicating the required case for the file name.

end Fname.UF;
