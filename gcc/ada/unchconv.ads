------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  U N C H E C K E D _ C O N V E R S I O N                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

generic
   type Source (<>) is limited private;
   type Target (<>) is limited private;

function Unchecked_Conversion (S : Source) return Target;
pragma Import (Intrinsic, Unchecked_Conversion);
pragma Pure (Unchecked_Conversion);
