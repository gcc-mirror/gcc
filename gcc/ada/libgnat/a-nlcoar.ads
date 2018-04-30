------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     ADA.NUMERICS.LONG_COMPLEX_ARRAYS                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Long_Real_Arrays;
with Ada.Numerics.Long_Complex_Types;

package Ada.Numerics.Long_Complex_Arrays is new
   Ada.Numerics.Generic_Complex_Arrays (Long_Real_Arrays, Long_Complex_Types);

pragma Pure (Long_Complex_Arrays);
