------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                       A D A . C O N T A I N E R S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

pragma Check_Name (Container_Checks);
pragma Check_Name (Tampering_Check);
--  The above checks are not in the Ada RM. They are added in order to allow
--  suppression of checks within containers packages. Suppressing
--  Tampering_Check suppresses the tampering checks and associated machinery,
--  which is very expensive. Suppressing Container_Checks suppresses
--  Tampering_Check as well as all the other (not-so-expensive) containers
--  checks.

package Ada.Containers is
   pragma Pure;

   type Hash_Type is mod 2**32;
   type Count_Type is range 0 .. 2**31 - 1;

   Capacity_Error : exception;

end Ada.Containers;
