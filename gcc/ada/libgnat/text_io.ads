------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                              T E X T _ I O                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This package is provided for compatibility with Ada 83. It renames
--  :ref:`Ada.Text_IO` and provides the same functionality.

pragma Ada_2012;
--  Explicit setting of Ada 2012 mode is required here, since we want to with a
--  child unit (not possible in Ada 83 mode), and Text_IO is not considered to
--  be an internal unit that is automatically compiled in Ada 2012 mode (since
--  a user is allowed to redeclare Text_IO).

with Ada.Text_IO;

package Text_IO renames Ada.Text_IO;
