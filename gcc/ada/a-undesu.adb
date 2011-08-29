------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     A D A . U N C H E C K E D _ D E A L L O C A T E _ S U B P O O L      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011, Free Software Foundation, Inc.            --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  ??? What is the header version here, see a-uncdea.adb. No GPL?

with System.Storage_Pools.Subpools,
     System.Storage_Pools.Subpools.Finalization;

use System.Storage_Pools.Subpools,
    System.Storage_Pools.Subpools.Finalization;

procedure Ada.Unchecked_Deallocate_Subpool
  (Subpool : in out System.Storage_Pools.Subpools.Subpool_Handle)
is
begin
   Finalize_And_Deallocate (Subpool);
end Ada.Unchecked_Deallocate_Subpool;
