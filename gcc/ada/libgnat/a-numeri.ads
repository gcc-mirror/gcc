------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . N U M E R I C S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This package is the top of the Ada.Numerics hierarchy as defined by ARM
--  A.5. It provides an exception used by all children and basic mathematical
--  constants.

package Ada.Numerics is
   pragma Pure;

   Argument_Error : exception;
   --  The Argument_Error exception is raised whenever an invalid
   --  value has passed as an argument to the subprograms defined
   --  this this package and its children.

   Pi : constant :=
          3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37511;
   --  The mathematical constant Pi

   e : constant :=
         2.71828_18284_59045_23536_02874_71352_66249_77572_47093_69996;
   --  The mathematical constant e

end Ada.Numerics;
