------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--      A D A . N U M E R I C S . S H O R T _ C O M P L E X _ T Y P E S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Complex_Types;

package Ada.Numerics.Short_Complex_Types is
   new Ada.Numerics.Generic_Complex_Types (Short_Float);

pragma Pure (Short_Complex_Types);
