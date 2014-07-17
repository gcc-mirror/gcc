------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . T E X T _ I O . E N U M E R A T I O N _ I O            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  In Ada 95, the package Ada.Text_IO.Enumeration_IO is a subpackage of
--  Text_IO. This is for compatibility with Ada 83. In GNAT we make it a
--  child package to avoid loading the necessary code if Enumeration_IO is
--  not instantiated. See routine Rtsfind.Check_Text_IO_Special_Unit for a
--  description of how we patch up the difference in semantics so that it
--  is invisible to the Ada programmer.

private generic
   type Enum is (<>);

package Ada.Text_IO.Enumeration_IO is

   Default_Width : Field := 0;
   Default_Setting : Type_Set := Upper_Case;

   procedure Get (File : File_Type; Item : out Enum);
   procedure Get (Item : out Enum);

   procedure Put
     (File  : File_Type;
      Item  : Enum;
      Width : Field := Default_Width;
      Set   : Type_Set := Default_Setting);

   procedure Put
     (Item  : Enum;
      Width : Field := Default_Width;
      Set   : Type_Set := Default_Setting);

   procedure Get
     (From : String;
      Item : out Enum;
      Last : out Positive);

   procedure Put
     (To   : out String;
      Item : Enum;
      Set  : Type_Set := Default_Setting);

end Ada.Text_IO.Enumeration_IO;
