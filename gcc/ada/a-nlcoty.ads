------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--       A D A . N U M E R I C S . L O N G _ C O M P L E X _ T Y P E S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Complex_Types;

package Ada.Numerics.Long_Complex_Types is
   new Ada.Numerics.Generic_Complex_Types (Long_Float);

pragma Pure (Long_Complex_Types);
