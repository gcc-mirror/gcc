------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . W I D E _ C H A R A C T E R S . H A N D L I N G          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Wide_Characters.Handling is
   pragma Pure;

   function Character_Set_Version return String;
   pragma Inline (Character_Set_Version);
   --  Returns an implementation-defined identifier that identifies the version
   --  of the character set standard that is used for categorizing characters
   --  by the implementation. For GNAT this is "Unicode v.v".

   function Is_Control (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Control);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  other_control, otherwise returns False.

   function Is_Letter (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Letter);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  letter_uppercase, letter_lowercase, letter_titlecase, letter_modifier,
   --  letter_other, or number_letter. Otherwise returns False.

   function Is_Lower (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Lower);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  letter_lowercase, otherwise returns False.

   function Is_Upper (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Upper);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  letter_uppercase, otherwise returns False.

   function Is_Digit (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Digit);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  number_decimal, otherwise returns False.

   function Is_Decimal_Digit (Item : Wide_Character) return Boolean
     renames Is_Digit;

   function Is_Hexadecimal_Digit (Item : Wide_Character) return Boolean;
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  number_decimal, or is in the range 'A' .. 'F' or 'a' .. 'f', otherwise
   --  returns False.

   function Is_Alphanumeric (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Alphanumeric);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  letter_uppercase, letter_lowercase, letter_titlecase, letter_modifier,
   --  letter_other, number_letter, or number_decimal; otherwise returns False.

   function Is_Special (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Special);
   --  Returns True if the Wide_Character designated by Item is categorized
   --  as graphic_character, but not categorized as letter_uppercase,
   --  letter_lowercase, letter_titlecase, letter_modifier, letter_other,
   --  number_letter, or number_decimal. Otherwise returns False.

   function Is_Line_Terminator (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Line_Terminator);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  separator_line or separator_paragraph, or if Item is a conventional line
   --  terminator character (CR, LF, VT, or FF). Otherwise returns False.

   function Is_Mark (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Mark);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  mark_non_spacing or mark_spacing_combining, otherwise returns False.

   function Is_Other_Format (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Other_Format);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  other_format, otherwise returns False.

   function Is_Punctuation_Connector (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Punctuation_Connector);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  punctuation_connector, otherwise returns False.

   function Is_Space (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Space);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  separator_space, otherwise returns False.

   function Is_Graphic (Item : Wide_Character) return Boolean;
   pragma Inline (Is_Graphic);
   --  Returns True if the Wide_Character designated by Item is categorized as
   --  graphic_character, otherwise returns False.

   function To_Lower (Item : Wide_Character) return Wide_Character;
   pragma Inline (To_Lower);
   --  Returns the Simple Lowercase Mapping of the Wide_Character designated by
   --  Item. If the Simple Lowercase Mapping does not exist for the
   --  Wide_Character designated by Item, then the value of Item is returned.

   function To_Lower (Item : Wide_String) return Wide_String;
   --  Returns the result of applying the To_Lower Wide_Character to
   --  Wide_Character conversion to each element of the Wide_String designated
   --  by Item. The result is the null Wide_String if the value of the formal
   --  parameter is the null Wide_String.

   function To_Upper (Item : Wide_Character) return Wide_Character;
   pragma Inline (To_Upper);
   --  Returns the Simple Uppercase Mapping of the Wide_Character designated by
   --  Item. If the Simple Uppercase Mapping does not exist for the
   --  Wide_Character designated by Item, then the value of Item is returned.

   function To_Upper (Item : Wide_String) return Wide_String;
   --  Returns the result of applying the To_Upper Wide_Character to
   --  Wide_Character conversion to each element of the Wide_String designated
   --  by Item. The result is the null Wide_String if the value of the formal
   --  parameter is the null Wide_String.

end Ada.Wide_Characters.Handling;
