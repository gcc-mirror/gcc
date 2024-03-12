------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               ADA.NUMERICS.LONG_LONG_ELEMENTARY_FUNCTIONS                --
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

package Ada.Numerics.Long_Long_Elementary_Functions is
  new Ada.Numerics.Generic_Elementary_Functions (Long_Long_Float);

pragma Pure (Long_Long_Elementary_Functions);
