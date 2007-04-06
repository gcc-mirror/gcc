------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--             A D A . S T R I N G S . B O U N D E D . H A S H              --
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

generic
   with package Bounded is
     new Ada.Strings.Bounded.Generic_Bounded_Length (<>);

function Ada.Strings.Bounded.Hash (Key : Bounded.Bounded_String)
    return Containers.Hash_Type;

pragma Preelaborate (Ada.Strings.Bounded.Hash);
