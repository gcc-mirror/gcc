------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--            ADA.NUMERICS.GENERIC_COMPLEX.ELEMENTARY_FUNCTIONS             --
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

with Ada.Numerics.Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;

package Ada.Numerics.Complex_Elementary_Functions is
  new Ada.Numerics.Generic_Complex_Elementary_Functions
                                            (Ada.Numerics.Complex_Types);

pragma Pure (Ada.Numerics.Complex_Elementary_Functions);
