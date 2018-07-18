------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--        A D A . S T R I N G S . W I D E _ W I D E _ F I X E D .           --
--                      W I D E _ W I D E _ H A S H                         --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers;
with Ada.Strings.Wide_Wide_Hash;

function Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash
 (Key : Wide_Wide_String) return Containers.Hash_Type
 renames Ada.Strings.Wide_Wide_Hash;

pragma Pure (Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash);
