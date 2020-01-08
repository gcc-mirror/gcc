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

--  Note that some Ada 2020 aspects are commented out since they are not
--  supported yet.

package Ada.Numerics.Big_Numbers
  --  with Pure, Nonblocking, Global => null
  with Pure
is
   subtype Field is Integer range 0 .. 255;
   subtype Number_Base is Integer range 2 .. 16;
end Ada.Numerics.Big_Numbers;
