------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
-- A D A . S H O R T _ S H O R T _ I N T E G E R _ W I D E _ T E X T _ I O  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Wide_Text_IO;

package Ada.Short_Short_Integer_Wide_Text_IO is
  new Ada.Wide_Text_IO.Integer_IO (Short_Short_Integer);
