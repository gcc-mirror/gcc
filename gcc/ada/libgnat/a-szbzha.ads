------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.STRINGS.WIDE_WIDE_BOUNDED.WIDE_WIDE_HASH                --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  Is this really an RM unit? doc needed ???

with Ada.Containers;

generic
   with package Bounded is
     new Ada.Strings.Wide_Wide_Bounded.Generic_Bounded_Length (<>);

function Ada.Strings.Wide_Wide_Bounded.Wide_Wide_Hash
  (Key : Bounded.Bounded_Wide_Wide_String)
  return Containers.Hash_Type;

pragma Preelaborate (Ada.Strings.Wide_Wide_Bounded.Wide_Wide_Hash);
