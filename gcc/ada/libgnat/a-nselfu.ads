------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 ADA.NUMERICS.SHORT_ELEMENTARY_FUNCTIONS                  --
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

package Ada.Numerics.Short_Elementary_Functions is
  new Ada.Numerics.Generic_Elementary_Functions (Short_Float);

pragma Pure (Short_Elementary_Functions);
pragma Annotate (GNATprove, Always_Return, Short_Elementary_Functions);
