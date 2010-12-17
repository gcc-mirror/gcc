------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       A D A . A S S E R T I O N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  We do a with of System.Assertions to get hold of the exception (following
--  the specific RM permission that lets' Assertion_Error being a renaming).
--  The suppression of Warnings stops the warning about bad categorization.

pragma Warnings (Off);
with System.Assertions;
pragma Warnings (On);

package Ada.Assertions is
   pragma Pure (Assertions);

   Assertion_Error : exception renames System.Assertions.Assert_Failure;
   --  This is the renaming that is allowed by 11.4.2(24). Note that the
   --  Exception_Name will refer to the one in System.Assertions (see
   --  AARM-11.4.1(12.b)).

   procedure Assert (Check : Boolean);

   procedure Assert (Check : Boolean; Message : String);

end Ada.Assertions;
