------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--           ADA.NUMERICS.LONG_LONG_COMPLEX.ELEMENTARY_FUNCTIONS            --
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

with Ada.Numerics.Long_Long_Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;

package Ada.Numerics.Long_Long_Complex_Elementary_Functions is
  new Ada.Numerics.Generic_Complex_Elementary_Functions
                                   (Ada.Numerics.Long_Long_Complex_Types);
