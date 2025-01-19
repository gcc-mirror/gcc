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

--  This package defines the base types used for big numbers in Ada as defined
--  by ARM A.5.5.

package Ada.Numerics.Big_Numbers
  with Pure
is
   subtype Field is Integer range 0 .. 255;
   --  The width of a big number. This is used when converting
   --  the internal representation into a string.

   subtype Number_Base is Integer range 2 .. 16;
   --  The base of a big number. This is used when converting
   --  the internal representation into a string.

end Ada.Numerics.Big_Numbers;
