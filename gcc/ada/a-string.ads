------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          A D A . S T R I N G S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Strings is
   pragma Pure;

   Space           : constant Character           := ' ';
   Wide_Space      : constant Wide_Character      := ' ';

   --  The following declaration is for Ada 2005 (AI-285)

   Wide_Wide_Space : constant Wide_Wide_Character := ' ';
   pragma Ada_05 (Wide_Wide_Space);

   Length_Error, Pattern_Error, Index_Error, Translation_Error : exception;

   type Alignment  is (Left, Right, Center);
   type Truncation is (Left, Right, Error);
   type Membership is (Inside, Outside);
   type Direction  is (Forward, Backward);
   type Trim_End   is (Left, Right, Both);

end Ada.Strings;
