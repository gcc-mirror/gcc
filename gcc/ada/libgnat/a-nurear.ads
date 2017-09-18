------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . N U M E R I C S . R E A L _ A R R A Y S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Real_Arrays;

package Ada.Numerics.Real_Arrays is
   new Ada.Numerics.Generic_Real_Arrays (Float);

pragma Pure (Real_Arrays);
