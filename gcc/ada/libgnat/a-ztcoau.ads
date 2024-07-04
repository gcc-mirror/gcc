------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . W I D E _ W I D E _ T E X T _ I O . C O M P L E X _ A U X     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routines for Ada.Wide_Wide_Text_IO.Complex_IO
--  that are shared among separate instantiations of this package. The routines
--  in this package are identical semantically to those in Complex_IO, except
--  that the generic parameter Complex has been replaced by separate real and
--  imaginary parameters, and default parameters have been removed because they
--  are supplied explicitly by the calls from within the generic template.

with Ada.Wide_Wide_Text_IO.Float_Aux;

private generic

   type Num is digits <>;

   with package Aux is new Ada.Wide_Wide_Text_IO.Float_Aux (Num, others => <>);

package Ada.Wide_Wide_Text_IO.Complex_Aux is

   procedure Get
     (File  : File_Type;
      ItemR : out Num;
      ItemI : out Num;
      Width : Field);

   procedure Put
     (File  : File_Type;
      ItemR : Num;
      ItemI : Num;
      Fore  : Field;
      Aft   : Field;
      Exp   : Field);

   procedure Gets
     (From  : String;
      ItemR : out Num;
      ItemI : out Num;
      Last  : out Positive);

   procedure Puts
     (To    : out String;
      ItemR : Num;
      ItemI : Num;
      Aft   : Field;
      Exp   : Field);

end Ada.Wide_Wide_Text_IO.Complex_Aux;
