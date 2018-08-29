------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     ADA.STRINGS.UTF_ENCODING.STRINGS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This is an Ada 2012 package defined in AI05-0137-1. It is used for encoding
--  and decoding String values using UTF encodings. Note: this package is
--  consistent with Ada 95, and may be included in Ada 95 implementations.

package Ada.Strings.UTF_Encoding.Strings is
   pragma Pure (Strings);

   --  The encoding routines take a String as input and encode the result
   --  using the specified UTF encoding method. The result includes a BOM if
   --  the Output_BOM argument is set to True. All 256 values of type Character
   --  are valid, so Encoding_Error cannot be raised for string input data.

   function Encode
     (Item          : String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM    : Boolean  := False) return UTF_String;
   --  Encode String using UTF-8, UTF-16LE or UTF-16BE encoding as specified by
   --  the Output_Scheme parameter.

   function Encode
     (Item       : String;
      Output_BOM : Boolean  := False) return UTF_8_String;
   --  Encode String using UTF-8 encoding

   function Encode
     (Item       : String;
      Output_BOM : Boolean  := False) return UTF_16_Wide_String;
   --  Encode String using UTF_16 encoding

   --  The decoding routines take a UTF String as input, and return a decoded
   --  Wide_String. If the UTF String starts with a BOM that matches the
   --  encoding method, it is ignored. An incorrect BOM raises Encoding_Error,
   --  as does a code out of range of type Character.

   function Decode
     (Item         : UTF_String;
      Input_Scheme : Encoding_Scheme) return String;
   --  The input is encoded in UTF_8, UTF_16LE or UTF_16BE as specified by the
   --  Input_Scheme parameter. It is decoded and returned as a String value.
   --  Note: a convenient form for scheme may be Encoding (UTF_String).

   function Decode
     (Item : UTF_8_String) return String;
   --  The input is encoded in UTF-8 and returned as a String value

   function Decode
     (Item : UTF_16_Wide_String) return String;
   --  The input is encoded in UTF-16 and returned as a String value

end Ada.Strings.UTF_Encoding.Strings;
