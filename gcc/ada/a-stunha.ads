------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                        ADA.STRINGS.UNBOUNDED.HASH                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2004 Free Software Foundation, Inc.            --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers;

function Ada.Strings.Unbounded.Hash
  (Key : Unbounded_String) return Containers.Hash_Type;

pragma Preelaborate (Ada.Strings.Unbounded.Hash);
