------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S E Q U E N T I A L  _ I O                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;
--  Explicit setting of Ada 2005 mode is required here, since we want to with a
--  child unit (not possible in Ada 83 mode), and Text_IO is not considered to
--  be an internal unit that is automatically compiled in Ada 2005 mode (since
--  a user is allowed to redeclare Sequential_IO).

with Ada.Sequential_IO;

generic package Sequential_IO renames Ada.Sequential_IO;
