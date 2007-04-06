------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--A D A . L O N G _ L O N G _ F L O A T _ W I D E _ W I D E _ T E X T _ I O --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

package Ada.Long_Long_Float_Wide_Wide_Text_IO is
  new Ada.Wide_Wide_Text_IO.Float_IO (Long_Long_Float);
