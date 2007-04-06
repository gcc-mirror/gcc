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

package Ada.Numerics is
   pragma Pure;

   Argument_Error : exception;

   Pi : constant :=
          3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37511;

   ["03C0"] : constant := Pi;
   --  This is the greek letter Pi (for Ada 2005 AI-388). Note that it is
   --  conforming to have this constant present even in Ada 95 mode, as there
   --  is no way for a normal mode Ada 95 program to reference this identifier.

   e : constant :=
         2.71828_18284_59045_23536_02874_71352_66249_77572_47093_69996;

end Ada.Numerics;
