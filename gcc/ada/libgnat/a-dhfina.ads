------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  ADA.DIRECTORIES.HIERARCHICAL_FILE_NAMES                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Directories.Hierarchical_File_Names is
   pragma Unimplemented_Unit;

   function Is_Simple_Name (Name : String) return Boolean;

   function Is_Root_Directory_Name (Name : String) return Boolean;

   function Is_Parent_Directory_Name (Name : String) return Boolean;

   function Is_Current_Directory_Name (Name : String) return Boolean;

   function Is_Full_Name (Name : String) return Boolean;

   function Is_Relative_Name (Name : String) return Boolean;

   function Simple_Name (Name : String) return String
     renames Ada.Directories.Simple_Name;

   function Containing_Directory (Name : String) return String
     renames Ada.Directories.Containing_Directory;

   function Initial_Directory (Name : String) return String;

   function Relative_Name (Name : String) return String;

   function Compose
     (Directory      : String := "";
      Relative_Name  : String;
      Extension      : String := "") return String;

end Ada.Directories.Hierarchical_File_Names;
