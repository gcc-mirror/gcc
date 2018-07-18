------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--        A D A . W I D E _ W I D E _ T E X T _ I O . F L O A T _ I O       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  In Ada 95, the package Ada.Wide_Wide_Text_IO.Float_IO is a subpackage of
--  Wide_Wide_Text_IO. In GNAT we make it a child package to avoid loading
--  the necessary code if Float_IO is not instantiated. See the routine
--  Rtsfind.Check_Text_IO_Special_Unit for a description of how we patch up
--  the difference in semantics so that it is invisible to the Ada programmer.

private generic
   type Num is digits <>;

package Ada.Wide_Wide_Text_IO.Float_IO is

   Default_Fore : Field := 2;
   Default_Aft  : Field := Num'Digits - 1;
   Default_Exp  : Field := 3;

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0);

   procedure Get
     (Item  : out Num;
      Width : Field := 0);

   procedure Put
     (File : File_Type;
      Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

   procedure Put
     (Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

   procedure Get
     (From : Wide_Wide_String;
      Item : out Num;
      Last : out Positive);

   procedure Put
     (To   : out Wide_Wide_String;
      Item : Num;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp);

end Ada.Wide_Wide_Text_IO.Float_IO;
