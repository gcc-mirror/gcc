------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . S T R I N G S . U T F _ E N C O D I N G             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Ada 2012 package defined in AI05-0137-1. It is used for
--  encoding strings using UTF encodings (UTF-8, UTF-16LE, UTF-16BE, UTF-16).

--  Compared with version 05 of the AI, we have added routines for UTF-16
--  encoding and decoding of wide strings, which seems missing from the AI,
--  added comments, and reordered the declarations.

--  Note: although this is an Ada 2012 package, the earlier versions of the
--  language permit the addition of new grandchildren of Ada, so we are able
--  to add this package unconditionally for use in Ada 2005 mode. We cannot
--  allow it in earlier versions, since it requires Wide_Wide_Character/String.

package Ada.Strings.UTF_Encoding is
   pragma Pure (UTF_Encoding);

   type Encoding_Scheme is (UTF_None, UTF_8, UTF_16BE, UTF_16LE, UTF_16);

   subtype Short_Encoding is Encoding_Scheme range UTF_8 .. UTF_16LE;
   subtype Long_Encoding  is Encoding_Scheme range UTF_16 .. UTF_16;

   --  The BOM (BYTE_ORDER_MARK) values defined here are used at the start of
   --  a string to indicate the encoding. The convention in this package is
   --  that decoding routines ignore a BOM, and output of encoding routines
   --  does not include a BOM. If you want to include a BOM in the output,
   --  you simply concatenate the appropriate value at the start of the string.

   BOM_8    : constant String :=
                Character'Val (16#EF#) &
                Character'Val (16#BB#) &
                Character'Val (16#BF#);

   BOM_16BE : constant String :=
                Character'Val (16#FE#) &
                Character'Val (16#FF#);

   BOM_16LE : constant String :=
                Character'Val (16#FF#) &
                Character'Val (16#FE#);

   BOM_16   : constant Wide_String :=
                (1 => Wide_Character'Val (16#FEFF#));

   --  The encoding routines take a wide string or wide wide string as input
   --  and encode the result using the specified UTF encoding method. For
   --  UTF-16, the output is returned as a Wide_String, this is not a normal
   --  Wide_String, since the codes in it may represent UTF-16 surrogate
   --  characters used to encode large values. Similarly for UTF-8, UTF-16LE,
   --  and UTF-16BE, the output is returned in a String, and again this String
   --  is not a standard format string, since it may include UTF-8 surrogates.
   --  As previously noted, the returned value does NOT start with a BOM.

   --  Note: invalid codes in calls to one of the Encode routines represent
   --  invalid values in the sense that they are not defined. For example, the
   --  code 16#DC03# is not a valid wide character value. Such values result
   --  in undefined behavior. For GNAT, Constraint_Error is raised with an
   --  appropriate exception message.

   function Encode
     (Item   : Wide_String;
      Scheme : Short_Encoding := UTF_8) return String;
   function Encode
     (Item   : Wide_Wide_String;
      Scheme : Short_Encoding := UTF_8) return String;

   function Encode
     (Item   : Wide_String;
      Scheme : Long_Encoding := UTF_16) return Wide_String;
   function Encode
     (Item   : Wide_Wide_String;
      Scheme : Long_Encoding := UTF_16) return Wide_String;

   --  The decoding routines take a String or Wide_String input which is an
   --  encoded string using the specified encoding. The output is a normal
   --  Ada Wide_String or Wide_Wide_String value representing the decoded
   --  values. Note that a BOM in the input matching the encoding is skipped.

   Encoding_Error : exception;
   --  Exception raised if an invalid encoding sequence is encountered by
   --  one of the Decode routines.

   function Decode
     (Item   : String;
      Scheme : Short_Encoding := UTF_8) return Wide_String;
   function Decode
     (Item   : String;
      Scheme : Short_Encoding := UTF_8) return Wide_Wide_String;

   function Decode
     (Item   : Wide_String;
      Scheme : Long_Encoding := UTF_16) return Wide_String;
   function Decode
     (Item   : Wide_String;
      Scheme : Long_Encoding := UTF_16) return Wide_Wide_String;

   --  The Encoding functions inspect an encoded string or wide_string and
   --  determine if a BOM is present. If so, the appropriate Encoding_Scheme
   --  is returned. If not, then UTF_None is returned.

   function Encoding (Item : String)      return Encoding_Scheme;
   function Encoding (Item : Wide_String) return Encoding_Scheme;

end Ada.Strings.UTF_Encoding;
