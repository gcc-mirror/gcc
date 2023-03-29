------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . W I D E _ T E X T _ I O . M O D U L A R _ I O          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  In Ada 95, the package Ada.Wide_Text_IO.Modular_IO is a subpackage
--  of Wide_Text_IO. In GNAT we make it a child package to avoid loading
--  the necessary code if Modular_IO is not instantiated. See the routine
--  Rtsfind.Check_Text_IO_Special_Unit for a description of how we patch up
--  the difference in semantics so that it is invisible to the Ada programmer.

private generic
   type Num is mod <>;

package Ada.Wide_Text_IO.Modular_IO is

   Default_Width : Field := Num'Width;
   Default_Base  : Number_Base := 10;

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0);

   procedure Get
     (Item  : out Num;
      Width : Field := 0);

   procedure Put
     (File  : File_Type;
      Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base);

   procedure Put
     (Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base);

   procedure Get
     (From : Wide_String;
      Item : out Num;
      Last : out Positive);

   procedure Put
     (To   : out Wide_String;
      Item : Num;
      Base : Number_Base := Default_Base);

end Ada.Wide_Text_IO.Modular_IO;
