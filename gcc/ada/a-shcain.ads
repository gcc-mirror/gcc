------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                    ADA.STRINGS.HASH_CASE_INSENSITIVE                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers;

function Ada.Strings.Hash_Case_Insensitive
  (Key : String) return Containers.Hash_Type;

pragma Pure (Ada.Strings.Hash_Case_Insensitive);
