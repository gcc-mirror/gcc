------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--        A D A . L O N G _ L O N G _ I N T E G E R _ T E X T _ I O         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;

package Ada.Long_Long_Integer_Text_IO is
  new Ada.Text_IO.Integer_IO (Long_Long_Integer);
