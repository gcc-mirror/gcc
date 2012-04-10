------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . W I D E _ W I D E _ C H A R A C T E R S . H A N D L I N G     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Wide_Wide_Characters.Handling is
   pragma Pure;
   --  This package is clearly intended to be Pure, by analogy with the
   --  base Ada.Characters.Handling package. The version in the RM does
   --  not yet have this pragma, but that is a clear omission. This will
   --  be fixed in a future version of AI05-0266-1.

   function Is_Control (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Control);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as other_control, otherwise returns false.

   function Is_Letter (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Letter);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as letter_uppercase, letter_lowercase, letter_titlecase,
   --  letter_modifier, letter_other, or number_letter. Otherwise returns
   --  false.

   function Is_Lower (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Lower);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as letter_lowercase, otherwise returns false.

   function Is_Upper (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Upper);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as letter_uppercase, otherwise returns false.

   function Is_Digit (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Digit);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as number_decimal, otherwise returns false.

   function Is_Decimal_Digit (Item : Wide_Wide_Character) return Boolean
     renames Is_Digit;

   function Is_Hexadecimal_Digit (Item : Wide_Wide_Character) return Boolean;
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as number_decimal, or is in the range 'A' .. 'F' or
   --  'a' .. 'f', otherwise returns false.

   function Is_Alphanumeric (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Alphanumeric);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as letter_uppercase, letter_lowercase, letter_titlecase,
   --  letter_modifier, letter_other, number_letter, or number_decimal.
   --  Otherwise returns false.

   function Is_Special (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Special);
   --  Returns True if the Wide_Wide_Character designated by Item
   --  is categorized as graphic_character, but not categorized as
   --  letter_uppercase, letter_lowercase, letter_titlecase, letter_modifier,
   --  letter_other, number_letter, or number_decimal. Otherwise returns false.

   function Is_Line_Terminator (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Line_Terminator);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as separator_line or separator_paragraph, or if Item is a
   --  conventional line terminator character (CR, LF, VT, or FF). Otherwise
   --  returns false.

   function Is_Mark (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Mark);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as mark_non_spacing or mark_spacing_combining, otherwise
   --  returns false.

   function Is_Other (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Other);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as other_format, otherwise returns false.

   function Is_Punctuation (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Punctuation);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as punctuation_connector, otherwise returns false.

   function Is_Space (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Space);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as separator_space, otherwise returns false.

   function Is_Graphic (Item : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Graphic);
   --  Returns True if the Wide_Wide_Character designated by Item is
   --  categorized as graphic_character, otherwise returns false.

   function To_Lower (Item : Wide_Wide_Character) return Wide_Wide_Character;
   pragma Inline (To_Lower);
   --  Returns the Simple Lowercase Mapping of the Wide_Wide_Character
   --  designated by Item. If the Simple Lowercase Mapping does not exist for
   --  the Wide_Wide_Character designated by Item, then the value of Item is
   --  returned.

   function To_Lower (Item : Wide_Wide_String) return Wide_Wide_String;
   --  Returns the result of applying the To_Lower Wide_Wide_Character to
   --  Wide_Wide_Character conversion to each element of the Wide_Wide_String
   --  designated by Item. The result is the null Wide_Wide_String if the value
   --  of the formal parameter is the null Wide_Wide_String.

   function To_Upper (Item : Wide_Wide_Character) return Wide_Wide_Character;
   pragma Inline (To_Upper);
   --  Returns the Simple Uppercase Mapping of the Wide_Wide_Character
   --  designated by Item. If the Simple Uppercase Mapping does not exist for
   --  the Wide_Wide_Character designated by Item, then the value of Item is
   --  returned.

   function To_Upper (Item : Wide_Wide_String) return Wide_Wide_String;
   --  Returns the result of applying the To_Upper Wide_Wide_Character to
   --  Wide_Wide_Character conversion to each element of the Wide_Wide_String
   --  designated by Item. The result is the null Wide_Wide_String if the value
   --  of the formal parameter is the null Wide_Wide_String.

end Ada.Wide_Wide_Characters.Handling;
