------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   ADA.STRINGS.UTF_ENCODING.CONVERSIONS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This is an Ada 2012 package defined in AI05-0137-1. It provides conversions
--  from one UTF encoding method to another. Note: this package is consistent
--  with Ada 95, and may be used in Ada 95 or Ada 2005 mode.

package Ada.Strings.UTF_Encoding.Conversions is
   pragma Pure (Conversions);

   --  In the following conversion routines, a BOM in the input that matches
   --  the encoding scheme is ignored, an incorrect BOM causes Encoding_Error
   --  to be raised. A BOM is present in the output if the Output_BOM parameter
   --  is set to True.

   function Convert
     (Item          : UTF_String;
      Input_Scheme  : Encoding_Scheme;
      Output_Scheme : Encoding_Scheme;
      Output_BOM    : Boolean := False) return UTF_String;
   --  Convert from input encoded in UTF-8, UTF-16LE, or UTF-16BE as specified
   --  by the Input_Scheme argument, and generate an output encoded in one of
   --  these three schemes as specified by the Output_Scheme argument.

   function Convert
     (Item          : UTF_String;
      Input_Scheme  : Encoding_Scheme;
      Output_BOM    : Boolean := False) return UTF_16_Wide_String;
   --  Convert from input encoded in UTF-8, UTF-16LE, or UTF-16BE as specified
   --  by the Input_Scheme argument, and generate an output encoded in UTF-16.

   function Convert
     (Item          : UTF_8_String;
      Output_BOM    : Boolean := False) return UTF_16_Wide_String;
   --  Convert from UTF-8 to UTF-16

   function Convert
     (Item          : UTF_16_Wide_String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM    : Boolean := False) return UTF_String;
   --  Convert from UTF-16 to UTF-8, UTF-16LE, or UTF-16BE as specified by
   --  the Output_Scheme argument.

   function Convert
     (Item          : UTF_16_Wide_String;
      Output_BOM    : Boolean := False) return UTF_8_String;
   --  Convert from UTF-16 to UTF-8

end Ada.Strings.UTF_Encoding.Conversions;
