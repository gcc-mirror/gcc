------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                           A D A . L O C A L E S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2010, Free Software Foundation, Inc.            --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Locales is
   pragma Preelaborate (Locales);
   pragma Remote_Types (Locales);

   type Language_Code is array (1 .. 3) of Character range 'a' .. 'z';
   type Country_Code  is array (1 .. 2) of Character range 'A' .. 'Z';

   Language_Unknown : constant Language_Code := "und";
   Country_Unknown  : constant Country_Code := "ZZ";

   function Language return Language_Code;
   function Country return Country_Code;

end Ada.Locales;
