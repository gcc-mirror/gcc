------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                        ADA.CONTAINERS.HASH_TABLES                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Finalization;

package Ada.Containers.Hash_Tables is
pragma Preelaborate;

   generic
      type Node_Access is private;

   package Generic_Hash_Table_Types is
      type Buckets_Type is array (Hash_Type range <>) of Node_Access;

      type Buckets_Access is access Buckets_Type;

      type Hash_Table_Type is new Ada.Finalization.Controlled with record
         Buckets : Buckets_Access;
         Length  : Count_Type := 0;
      end record;
   end Generic_Hash_Table_Types;

end Ada.Containers.Hash_Tables;
