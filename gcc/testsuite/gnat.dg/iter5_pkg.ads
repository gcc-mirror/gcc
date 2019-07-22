with Ada.Calendar;
with Ada.Directories;

with Ada.Iterator_Interfaces;

package Iter5_Pkg is

  subtype Size is Ada.Directories.File_Size;

  type Folder is new String;

  function Folder_Separator return Character;

  function "+" (Directory : String) return Folder;

  function "+" (Left, Right : String) return Folder;

  function "+" (Left  : Folder;
                Right : String) return Folder;

  function Composure (Directory : Folder;
                      Filename  : String;
                      Extension : String) return String;

  function Composure (Directory : String;
                      Filename  : String;
                      Extension : String) return String;
  -- no exception

  function Base_Name_Of (Name : String) return String
    renames Ada.Directories.Base_Name;

  function Extension_Of (Name : String) return String
    renames Ada.Directories.Extension;

  function Containing_Directory_Of (Name : String) return String
    renames Ada.Directories.Containing_Directory;

  function Exists (Name : String) return Boolean;
  -- no exception

  function Size_Of (Name : String) return Size renames Ada.Directories.Size;

  function Directory_Exists (Name : String) return Boolean;
  -- no exception

  function Modification_Time_Of (Name : String) return Ada.Calendar.Time
    renames Ada.Directories.Modification_Time;

  function Is_Newer (The_Name  : String;
                     Than_Name : String) return Boolean;

  procedure Delete (Name : String);
  -- no exception if no existance

  procedure Create_Directory (Path : String);
  -- creates the whole directory path

  procedure Delete_Directory (Name : String); -- including contents
  -- no exception if no existance

  procedure Rename (Old_Name : String;
                    New_Name : String) renames Ada.Directories.Rename;

  procedure Copy (Source_Name   : String;
                  Target_Name   : String;
                  Form          : String := "")
    renames Ada.Directories.Copy_File;

  function Is_Leaf_Directory (Directory : String) return Boolean;

  procedure Iterate_Over_Leaf_Directories (From_Directory : String;
                                           Iterator : access procedure
                                             (Leaf_Directory : String));

  function Found_Directory (Simple_Name  : String;
                            In_Directory : String) return String;

  Not_Found : exception;

  Name_Error : exception renames Ada.Directories.Name_Error;
  Use_Error  : exception renames Ada.Directories.Use_Error;

  ------------------------
  -- File Iterator Loop --
  ------------------------
  -- Example:
  --          for The_Filename of Iter5_Pkg.Iterator_For ("C:\Program_Files") loop
  --            Log.Write (The_Filename);
  --          end loop;

  type Item (Name_Length : Natural) is limited private;

  function Iterator_For (Name : String) return Item;

private
  type Cursor;

  function Has_More (Data : Cursor) return Boolean;

  package List_Iterator_Interfaces is
    new Ada.Iterator_Interfaces (Cursor, Has_More);

  function Iterate (The_Item : Item)
    return List_Iterator_Interfaces.Forward_Iterator'class;

  type Cursor_Data is record
    Has_More : Boolean := False;
    Position : Ada.Directories.Search_Type;
  end record;

  type Cursor is access all Cursor_Data;

  function Constant_Reference (The_Item     : aliased Item;
                               Unused_Index : Cursor) return String;

  type Item (Name_Length : Natural) is tagged limited record
    Name   : String(1..Name_Length);
    Actual : Ada.Directories.Directory_Entry_Type;
    Data   : aliased Cursor_Data;
  end record
  with
    Constant_Indexing => Constant_Reference,
    Default_Iterator  => Iterate,
    Iterator_Element  => String;

end Iter5_Pkg;
