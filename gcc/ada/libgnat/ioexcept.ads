------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        I O _ E X C E P T I O N S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;
--  Explicit setting of Ada 2012 mode is required here, since we want to with a
--  child unit (not possible in Ada 83 mode), and IO_Exceptions is not
--  considered to be an internal unit that is automatically compiled in Ada
--  2012 mode (since a user is allowed to redeclare IO_Exceptions).

with Ada.IO_Exceptions;

package IO_Exceptions renames Ada.IO_Exceptions;
