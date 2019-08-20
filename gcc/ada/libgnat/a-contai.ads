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

package Ada.Containers is
   pragma Pure;

   type Hash_Type is mod 2**32;
   --  Represents the range of the result of a hash function

   type Count_Type is range 0 .. 2**31 - 1;
   --  Represents the (potential or actual) number of elements of a container

   Capacity_Error : exception;
   --  Raised when the capacity of a container is exceeded

end Ada.Containers;
