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

with System.Storage_Pools.Subpools; use System.Storage_Pools.Subpools;

procedure Ada.Unchecked_Deallocate_Subpool
  (Subpool : in out System.Storage_Pools.Subpools.Subpool_Handle)
is
begin
   --  Finalize all controlled objects allocated on the input subpool

   --  ??? It is awkward to create a child of Storage_Pools.Subpools for the
   --  sole purpose of exporting Finalize_Subpool.

--   Finalize_Subpool (Subpool);

   --  Dispatch to the user-defined implementation of Deallocate_Subpool

   Deallocate_Subpool (Pool_Of_Subpool (Subpool).all, Subpool);
end Ada.Unchecked_Deallocate_Subpool;
