------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . L O A D                              --
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

--  This child package contains the function used to load a separately
--  compiled unit, as well as the routine used to initialize the unit
--  table and load the main source file.

package Lib.Load is

   -------------------------------
   -- Handling of Renamed Units --
   -------------------------------

   --  A compilation unit can be a renaming of another compilation unit.
   --  Such renamed units are not allowed as parent units, that is you
   --  cannot declare a unit:

   --     with x;
   --     package x.y is end;

   --  where x is a renaming of some other package. However you can refer
   --  to a renamed unit in a with clause:

   --     package p is end;

   --     package p.q is end;

   --     with p;
   --     package pr renames p;

   --     with pr.q ....

   --  This means that in the context of a with clause, the normal fixed
   --  correspondence between unit and file names is broken. In the above
   --  example, there is no file named pr-q.ads, since the actual child
   --  unit is p.q, and it will be found in file p-q.ads.

   --  In order to deal with this case, we have to first load pr.ads, and
   --  then discover that it is a renaming of p, so that we know that pr.q
   --  really refers to p.q. Furthermore this can happen at any level:

   --     with p.q;
   --     package p.r renames p.q;

   --     with p.q;
   --     package p.q.s is end;

   --     with p.r.s ...

   --  Now we have a case where the parent p.r is a child unit and is
   --  a renaming. This shows that renaming can occur at any level.

   --  Finally, consider:

   --     with pr.q.s ...

   --  Here the parent pr.q is not itself a renaming, but it really refers
   --  to the unit p.q, and again we cannot know this without loading the
   --  parent. The bottom line here is that while the file name of a unit
   --  always corresponds to the unit name, the unit name as given to the
   --  Load_Unit function may not be the real unit.

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize internal tables

   procedure Initialize_Version (U : Unit_Number_Type);
   --  This is called once the source file corresponding to unit U has been
   --  fully scanned. At that point the checksum is computed, and can be used
   --  to initialize the version number.

   procedure Load_Main_Source;
   --  Called at the start of compiling a new main source unit to initialize
   --  the library processing for the new main source. Establishes and
   --  initializes the units table entry for the new main unit (leaving
   --  the Unit_File_Name entry of Main_Unit set to No_File if there are no
   --  more files. Otherwise the main source file has been opened and read
   --  and then closed on return.

   function Load_Unit
     (Load_Name  : Unit_Name_Type;
      Required   : Boolean;
      Error_Node : Node_Id;
      Subunit    : Boolean;
      Corr_Body  : Unit_Number_Type := No_Unit;
      Renamings  : Boolean          := False;
      With_Node  : Node_Id          := Empty;
      PMES       : Boolean          := False) return Unit_Number_Type;
   --  This function loads and parses the unit specified by Load_Name (or
   --  returns the unit number for the previously constructed units table
   --  entry if this is not the first call for this unit). Required indicates
   --  the behavior on a file not found condition, as further described below,
   --  and Error_Node is the node in the calling program to which error
   --  messages are to be attached.
   --
   --  If the corresponding file is found, the value returned by Load is the
   --  unit number that indexes the corresponding entry in the units table. If
   --  a serious enough parser error occurs to prevent subsequent semantic
   --  analysis, then the Fatal_Error flag of the returned entry is set and
   --  in addition, the fatal error flag of the calling unit is also set.
   --
   --  If the corresponding file is not found, then the behavior depends on
   --  the setting of Required. If Required is False, then No_Unit is returned
   --  and no error messages are issued. If Required is True, then an error
   --  message is posted, and No_Unit is returned.
   --
   --  A special case arises in the call from Rtsfind, where Error_Node is set
   --  to Empty. In this case Required is False, and the caller in any case
   --  treats any error as fatal.
   --
   --  The Subunit parameter is True to load a subunit, and False to load
   --  any other kind of unit (including all specs, package bodies, and
   --  subprogram bodies).
   --
   --  The Corr_Body argument is normally defaulted. It is set only in the
   --  case of loading the corresponding spec when the main unit is a body.
   --  In this case, Corr_Body is the unit number of this corresponding
   --  body. This is used to set the Serial_Ref_Unit field of the unit
   --  table entry. It is also used to deal with the special processing
   --  required by RM 10.1.4(4). See description in lib.ads.
   --
   --  Renamings activates the handling of renamed units as separately
   --  described in the documentation of this unit. If this parameter is
   --  set to True, then Load_Name may not be the real unit name and it
   --  is necessary to load parents to find the real name.
   --
   --  With_Node is set to the with_clause or limited_with_clause causing
   --  the unit to be loaded, and is used to bypass the circular dependency
   --  check in the case of a limited_with_clause (Ada 2005, AI-50217).
   --
   --  PMES indicates the required setting of Parsing_Main_Extended_Unit during
   --  loading of the unit. This flag is saved and restored over the call.
   --  Note: PMES is false for the subunit case, which seems wrong???

   procedure Change_Main_Unit_To_Spec;
   --  This procedure is called if the main unit file contains a No_Body pragma
   --  and no other tokens. The effect is, if possible, to change the main unit
   --  from the body it references now, to the corresponding spec. This has the
   --  effect of ignoring the body, which is what we want. If it is impossible
   --  to successfully make the change, then the call has no effect, and the
   --  file is unchanged (this will lead to an error complaining about the
   --  inappropriate No_Body spec).

   function Create_Dummy_Package_Unit
     (With_Node : Node_Id;
      Spec_Name : Unit_Name_Type) return Unit_Number_Type;
   --  With_Node is the Node_Id of a with statement for which the file could
   --  not be found, and Spec_Name is the corresponding unit name. This call
   --  creates a dummy package unit so that compilation can continue without
   --  blowing up when the missing unit is referenced.

   procedure Make_Child_Decl_Unit (N : Node_Id);
   --  For a child subprogram body without a spec, we create a subprogram
   --  declaration in order to attach the required parent link. We create
   --  a Units_Table entry for this declaration, in order to maintain a
   --  one-to-one correspondence between compilation units and table entries.

   procedure Make_Instance_Unit (N : Node_Id; In_Main : Boolean);
   --  When a compilation unit is an instantiation, it contains both the
   --  declaration and the body of the instance, each of which can have its
   --  own elaboration routine. The file itself corresponds to the declaration.
   --  We create an additional entry for the body, so that the binder can
   --  generate the proper elaboration calls to both. The argument N is the
   --  compilation unit node created for the body.
   --
   --  If the instance is not the main program, we still generate the instance
   --  body even though we do not generate code for it. In that case we still
   --  generate a compilation unit node for it, and we need to make an entry
   --  for it in the units table, so as to maintain a one-to-one mapping
   --  between table and nodes. The table entry is used among other things to
   --  provide a canonical traversal order for context units for CodePeer.
   --  The flag In_Main indicates whether the instance is the main unit.

   procedure Version_Update (U : Node_Id; From : Node_Id);
   --  This routine is called when unit U is found to be semantically
   --  dependent on unit From. It updates the version of U to register
   --  dependence on the version of From. The arguments are compilation
   --  unit nodes for the relevant library nodes.

end Lib.Load;
