------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . L O N G _ C O M P L E X _ T E X T _ I O             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  Ada 2005 AI-328

with Ada.Text_IO.Complex_IO;
with Ada.Numerics.Long_Complex_Types;

pragma Elaborate_All (Ada.Text_IO.Complex_IO);

package Ada.Long_Complex_Text_IO is
  new Ada.Text_IO.Complex_IO (Ada.Numerics.Long_Complex_Types);
