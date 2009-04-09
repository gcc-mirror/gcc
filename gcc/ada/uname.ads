------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U N A M E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009  Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Namet; use Namet;
with Types; use Types;

package Uname is

   ---------------------------
   -- Unit Name Conventions --
   ---------------------------

   --  Units are associated with a unique ASCII name as follows. First we
   --  have the fully expanded name of the unit, with lower case letters
   --  (except for the use of upper case letters for encoding upper half
   --  and wide characters, as described in Namet), and periods. Following
   --  this is one of the following suffixes:

   --    %s  for package/subprogram/generic declarations (specs)
   --    %b  for package/subprogram/generic bodies and subunits

   --  Unit names are stored in the names table, and referred to by the
   --  corresponding Name_Id values. The subtype Unit_Name, which is a
   --  synonym for Name_Id, is used to indicate that a Name_Id value that
   --  holds a unit name (as defined above) is expected.

   --  Note: as far as possible the conventions for unit names are encapsulated
   --  in this package. The one exception is that package Fname, which provides
   --  conversion routines from unit names to file names must be aware of the
   --  precise conventions that are used.

   -------------------
   -- Display Names --
   -------------------

   --  For display purposes, unit names are printed out with the suffix
   --  " (body)" for a body and " (spec)" for a spec. These formats are
   --  used for the Write_Unit_Name and Get_Unit_Name_String subprograms.

   -----------------
   -- Subprograms --
   -----------------

   function Get_Body_Name (N : Unit_Name_Type) return Unit_Name_Type;
   --  Given the name of a spec, this function returns the name of the
   --  corresponding body, i.e. characters %s replaced by %b

   function Get_Parent_Body_Name (N : Unit_Name_Type) return Unit_Name_Type;
   --  Given the name of a subunit, returns the name of the parent body

   function Get_Parent_Spec_Name (N : Unit_Name_Type) return Unit_Name_Type;
   --  Given the name of a child unit spec or body, returns the unit name
   --  of the parent spec. Returns No_Name if the given name is not the name
   --  of a child unit.

   procedure Get_External_Unit_Name_String (N : Unit_Name_Type);
   --  Given the name of a body or spec unit, this procedure places in
   --  Name_Buffer the name of the unit with periods replaced by double
   --  underscores. The spec/body indication is eliminated. The length
   --  of the stored name is placed in Name_Len. All letters are lower
   --  case, corresponding to the string used in external names.

   function Get_Spec_Name (N : Unit_Name_Type) return Unit_Name_Type;
   --  Given the name of a body, this function returns the name of the
   --  corresponding spec, i.e. characters %b replaced by %s

   function Get_Unit_Name (N : Node_Id) return Unit_Name_Type;
   --  This procedure returns the unit name that corresponds to the given node,
   --  which is one of the following:
   --
   --    N_Subprogram_Declaration         (spec) cases
   --    N_Package_Declaration
   --    N_Generic_Declaration
   --    N_With_Clause
   --    N_Function_Instantiation
   --    N_Package_Instantiation
   --    N_Procedure_Instantiation
   --    N_Pragma (Elaborate case)
   --
   --    N_Package_Body                   (body) cases
   --    N_Subprogram_Body
   --    N_Identifier
   --    N_Selected_Component
   --
   --    N_Subprogram_Body_Stub           (subunit) cases
   --    N_Package_Body_Stub
   --    N_Task_Body_Stub
   --    N_Protected_Body_Stub
   --    N_Subunit

   procedure Get_Unit_Name_String
     (N      : Unit_Name_Type;
      Suffix : Boolean := True);
   --  Places the display name of the unit in Name_Buffer and sets Name_Len to
   --  the length of the stored name, i.e. it uses the same interface as the
   --  Get_Name_String routine in the Namet package. The name is decoded and
   --  contains an indication of spec or body if Boolean parameter Suffix is
   --  True.

   function Is_Body_Name (N : Unit_Name_Type) return Boolean;
   --  Returns True iff the given name is the unit name of a body (i.e. if
   --  it ends with the characters %b).

   function Is_Child_Name (N : Unit_Name_Type) return Boolean;
   --  Returns True iff the given name is a child unit name (of either a
   --  body or a spec).

   function Is_Spec_Name (N : Unit_Name_Type) return Boolean;
   --  Returns True iff the given name is the unit name of a specification
   --  (i.e. if it ends with the characters %s).

   function Name_To_Unit_Name (N : Name_Id) return Unit_Name_Type;
   --  Given the Id of the Ada name of a unit, this function returns the
   --  corresponding unit name of the spec (by appending %s to the name).

   function New_Child
     (Old  : Unit_Name_Type;
      Newp : Unit_Name_Type) return Unit_Name_Type;
   --   Old is a child unit name (for either a body or spec). Newp is the
   --   unit name of the actual parent (this may be different from the
   --   parent in old). The returned unit name is formed by taking the
   --   parent name from Newp and the child unit name from Old, with the
   --   result being a body or spec depending on Old. For example:
   --
   --     Old    = A.B.C (body)
   --     Newp   = A.R (spec)
   --     result = A.R.C (body)
   --
   --   See spec of Load_Unit for extensive discussion of why this routine
   --   needs to be used (the call in the body of Load_Unit is the only one).

   function Uname_Ge (Left, Right : Unit_Name_Type) return Boolean;
   function Uname_Gt (Left, Right : Unit_Name_Type) return Boolean;
   function Uname_Le (Left, Right : Unit_Name_Type) return Boolean;
   function Uname_Lt (Left, Right : Unit_Name_Type) return Boolean;
   --  These functions perform lexicographic ordering of unit names. The
   --  ordering is suitable for printing, and is not quite a straightforward
   --  comparison of the names, since the convention is that specs appear
   --  before bodies. Note that the standard = and /= operators work fine
   --  because all unit names are hashed into the name table, so if two names
   --  are the same, they always have the same Name_Id value.

   procedure Write_Unit_Name (N : Unit_Name_Type);
   --  Given a unit name, this procedure writes the display name to the
   --  standard output file. Name_Buffer and Name_Len are set as described
   --  above for the Get_Unit_Name_String call on return.

end Uname;
