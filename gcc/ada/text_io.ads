------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                              T E X T _ I O                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;
--  Explicit setting of Ada 2005 mode is required here, since we want to with a
--  child unit (not possible in Ada 83 mode), and Text_IO is not considered to
--  be an internal unit that is automatically compiled in Ada 2005 mode (since
--  a user is allowed to redeclare Text_IO).

with Ada.Text_IO;

package Text_IO renames Ada.Text_IO;
