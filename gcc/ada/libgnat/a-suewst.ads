------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   ADA.STRINGS.UTF_ENCODING.WIDE_STRINGS                  --
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
--  and decoding Wide_String values using UTF encodings. Note: this package is
--  consistent with Ada 95, and may be included in Ada 95 implementations.

package Ada.Strings.UTF_Encoding.Wide_Strings is
   pragma Pure (Wide_Strings);

   --  The encoding routines take a Wide_String as input and encode the result
   --  using the specified UTF encoding method. The result includes a BOM if
   --  the Output_BOM argument is set to True. Encoding_Error is raised if an
   --  invalid character appears in the input. In particular the characters
   --  in the range 16#D800# .. 16#DFFF# are invalid because they conflict
   --  with UTF-16 surrogate encodings, and the characters 16#FFFE# and
   --  16#FFFF# are also invalid because they conflict with BOM codes.

   function Encode
     (Item          : Wide_String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM    : Boolean  := False) return UTF_String;
   --  Encode Wide_String using UTF-8, UTF-16LE or UTF-16BE encoding as
   --  specified by the Output_Scheme parameter.

   function Encode
     (Item       : Wide_String;
      Output_BOM : Boolean  := False) return UTF_8_String;
   --  Encode Wide_String using UTF-8 encoding

   function Encode
     (Item       : Wide_String;
      Output_BOM : Boolean  := False) return UTF_16_Wide_String;
   --  Encode Wide_String using UTF_16 encoding

   --  The decoding routines take a UTF String as input, and return a decoded
   --  Wide_String. If the UTF String starts with a BOM that matches the
   --  encoding method, it is ignored. An incorrect BOM raises Encoding_Error.

   function Decode
     (Item         : UTF_String;
      Input_Scheme : Encoding_Scheme) return Wide_String;
   --  The input is encoded in UTF_8, UTF_16LE or UTF_16BE as specified by the
   --  Input_Scheme parameter. It is decoded and returned as a Wide_String
   --  value. Note: a convenient form for scheme may be Encoding (UTF_String).

   function Decode
     (Item : UTF_8_String) return Wide_String;
   --  The input is encoded in UTF-8 and returned as a Wide_String value

   function Decode
     (Item : UTF_16_Wide_String) return Wide_String;
   --  The input is encoded in UTF-16 and returned as a Wide_String value

end Ada.Strings.UTF_Encoding.Wide_Strings;
