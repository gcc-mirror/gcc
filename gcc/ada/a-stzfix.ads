------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . S T R I N G S . W I D E _ W I D E _ F I X E D           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Wide_Wide_Maps;

package Ada.Strings.Wide_Wide_Fixed is
   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Copy Procedure for Wide_Wide_Strings of Possibly Different Lengths --
   ------------------------------------------------------------------------

   procedure Move
     (Source  : Wide_Wide_String;
      Target  : out Wide_Wide_String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Wide_Wide_Character  := Ada.Strings.Wide_Wide_Space);

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity) return Natural;

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural;

   function Index
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : Wide_Wide_String;
      Set     : Wide_Wide_Maps.Wide_Wide_Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural;
   pragma Ada_05 (Index);

   function Index_Non_Blank
     (Source : Wide_Wide_String;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Wide_Wide_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;
   pragma Ada_05 (Index_Non_Blank);

   function Count
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                  Wide_Wide_Maps.Identity)
      return Natural;

   function Count
     (Source  : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Natural;

   function Count
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural;

   procedure Find_Token
     (Source : Wide_Wide_String;
      Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);

   ----------------------------------------------
   -- Wide_Wide_String Translation Subprograms --
   ----------------------------------------------

   function Translate
     (Source  : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Wide_Wide_String;

   procedure Translate
     (Source  : in out Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping);

   function Translate
     (Source  : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
      return Wide_Wide_String;

   procedure Translate
     (Source  : in out Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function);

   -------------------------------------------------
   -- Wide_Wide_String Transformation Subprograms --
   -------------------------------------------------

   function Replace_Slice
     (Source : Wide_Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_Wide_String) return Wide_Wide_String;

   procedure Replace_Slice
     (Source  : in out Wide_Wide_String;
      Low     : Positive;
      High    : Natural;
      By      : Wide_Wide_String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Wide_Wide_Character  := Ada.Strings.Wide_Wide_Space);

   function Insert
     (Source   : Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String) return Wide_Wide_String;

   procedure Insert
     (Source   : in out Wide_Wide_String;
      Before   : Positive;
      New_Item : Wide_Wide_String;
      Drop     : Truncation := Error);

   function Overwrite
     (Source   : Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String) return Wide_Wide_String;

   procedure Overwrite
     (Source   : in out Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String;
      Drop     : Truncation := Right);

   function Delete
     (Source  : Wide_Wide_String;
      From    : Positive;
      Through : Natural) return Wide_Wide_String;

   procedure Delete
     (Source  : in out Wide_Wide_String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Ada.Strings.Wide_Wide_Space);

   -------------------------------------------
   -- Wide_Wide_String Selector Subprograms --
   -------------------------------------------

   function Trim
     (Source : Wide_Wide_String;
      Side   : Trim_End) return Wide_Wide_String;

   procedure Trim
     (Source  : in out Wide_Wide_String;
      Side    : Trim_End;
      Justify : Alignment      := Left;
      Pad     : Wide_Wide_Character := Wide_Wide_Space);

   function Trim
     (Source : Wide_Wide_String;
      Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right  : Wide_Wide_Maps.Wide_Wide_Character_Set)
      return Wide_Wide_String;

   procedure Trim
     (Source  : in out Wide_Wide_String;
      Left    : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right   : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Justify : Alignment := Ada.Strings.Left;
      Pad     : Wide_Wide_Character := Ada.Strings.Wide_Wide_Space);

   function Head
     (Source : Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Ada.Strings.Wide_Wide_Space)
      return Wide_Wide_String;

   procedure Head
     (Source  : in out Wide_Wide_String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Wide_Character := Ada.Strings.Wide_Wide_Space);

   function Tail
     (Source : Wide_Wide_String;
      Count  : Natural;
      Pad    : Wide_Wide_Character := Ada.Strings.Wide_Wide_Space)
      return Wide_Wide_String;

   procedure Tail
     (Source : in out Wide_Wide_String;
      Count  : Natural;
      Justify : Alignment := Left;
      Pad    : Wide_Wide_Character := Ada.Strings.Wide_Wide_Space);

   --------------------------------------------
   -- Wide_Wide_String Constructor Functions --
   --------------------------------------------

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_Character) return Wide_Wide_String;

   function "*"
     (Left  : Natural;
      Right : Wide_Wide_String) return Wide_Wide_String;

end Ada.Strings.Wide_Wide_Fixed;
