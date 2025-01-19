------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                           A D A . L O C A L E S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2010-2025, Free Software Foundation, Inc.         --
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

   --  A locale identifies a geopolitical place or region and its associated
   --  language, which can be used to determine other internationalization-
   --  related characteristics. The active locale is the locale associated with
   --  the partition of the current task.

   type Language_Code is new String (1 .. 3)
      with Dynamic_Predicate =>
         (for all E of Language_Code => E in 'a' .. 'z');
   --  Lower-case string representation of an ISO 639-3 alpha-3 code that
   --  identifies a language.

   type Country_Code is new String (1 .. 2)
      with Dynamic_Predicate =>
         (for all E of Country_Code => E in 'A' .. 'Z');
   --  Upper-case string representation of an ISO 3166-1 alpha-2 code that
   --  identifies a country.

   Language_Unknown : constant Language_Code := "und";
   Country_Unknown  : constant Country_Code := "ZZ";

   function Language return Language_Code;
   --  Returns the code of the language associated with the active locale. If
   --  the Language_Code associated with the active locale cannot be determined
   --  from the environment, then Language returns Language_Unknown.

   function Country return Country_Code;
   --  Returns the code of the country associated with the active locale. If
   --  the Country_Code associated with the active locale cannot be determined
   --  from the environment, then Country returns Country_Unknown.

end Ada.Locales;
