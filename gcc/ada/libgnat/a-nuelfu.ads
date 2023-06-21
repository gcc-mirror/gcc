------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--     A D A . N U M E R I C S . E L E M E N T A R Y _ F U N C T I O N S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_Functions;

package Ada.Numerics.Elementary_Functions is
  new Ada.Numerics.Generic_Elementary_Functions (Float);

pragma Pure (Elementary_Functions);
