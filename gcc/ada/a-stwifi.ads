------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               A D A . S T R I N G S . W I D E _ F I X E D                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Wide_Maps;

package Ada.Strings.Wide_Fixed is
   pragma Preelaborate;

   -------------------------------------------------------------------
   -- Copy Procedure for Wide_Strings of Possibly Different Lengths --
   -------------------------------------------------------------------

   procedure Move
     (Source  : Wide_String;
      Target  : out Wide_String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Wide_Character  := Ada.Strings.Wide_Space);

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural;

   function Index
     (Source  : Wide_String;
      Pattern : Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural;

   function Index
     (Source : Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;

   function Index
     (Source  : Wide_String;
      Pattern : Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : Wide_String;
      Pattern : Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : Wide_String;
      Set     : Wide_Maps.Wide_Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural;
   pragma Ada_05 (Index);

   function Index_Non_Blank
     (Source : Wide_String;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : Wide_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;
   pragma Ada_05 (Index_Non_Blank);

   function Count
     (Source  : Wide_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural;

   function Count
     (Source  : Wide_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural;

   function Count
     (Source : Wide_String;
      Set    : Wide_Maps.Wide_Character_Set) return Natural;

   procedure Find_Token
     (Source : Wide_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);

   -----------------------------------------
   -- Wide_String Translation Subprograms --
   -----------------------------------------

   function Translate
     (Source  : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping) return Wide_String;

   procedure Translate
     (Source  : in out Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping);

   function Translate
     (Source  : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Wide_String;

   procedure Translate
     (Source  : in out Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function);

   --------------------------------------------
   -- Wide_String Transformation Subprograms --
   --------------------------------------------

   function Replace_Slice
     (Source : Wide_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String) return Wide_String;

   procedure Replace_Slice
     (Source  : in out Wide_String;
      Low     : Positive;
      High    : Natural;
      By      : Wide_String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Wide_Character  := Ada.Strings.Wide_Space);

   function Insert
     (Source   : Wide_String;
      Before   : Positive;
      New_Item : Wide_String) return Wide_String;

   procedure Insert
     (Source   : in out Wide_String;
      Before   : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error);

   function Overwrite
     (Source   : Wide_String;
      Position : Positive;
      New_Item : Wide_String) return Wide_String;

   procedure Overwrite
     (Source   : in out Wide_String;
      Position : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Right);

   function Delete
     (Source  : Wide_String;
      From    : Positive;
      Through : Natural) return Wide_String;

   procedure Delete
     (Source  : in out Wide_String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Ada.Strings.Wide_Space);

   --------------------------------------
   -- Wide_String Selector Subprograms --
   --------------------------------------

   function Trim
     (Source : Wide_String;
      Side   : Trim_End) return Wide_String;

   procedure Trim
     (Source  : in out Wide_String;
      Side    : Trim_End;
      Justify : Alignment      := Left;
      Pad     : Wide_Character := Wide_Space);

   function Trim
     (Source : Wide_String;
      Left   : Wide_Maps.Wide_Character_Set;
      Right  : Wide_Maps.Wide_Character_Set) return Wide_String;

   procedure Trim
     (Source  : in out Wide_String;
      Left    : Wide_Maps.Wide_Character_Set;
      Right   : Wide_Maps.Wide_Character_Set;
      Justify : Alignment := Ada.Strings.Left;
      Pad     : Wide_Character := Ada.Strings.Wide_Space);

   function Head
     (Source : Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Ada.Strings.Wide_Space) return Wide_String;

   procedure Head
     (Source  : in out Wide_String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Wide_Character := Ada.Strings.Wide_Space);

   function Tail
     (Source : Wide_String;
      Count  : Natural;
      Pad    : Wide_Character := Ada.Strings.Wide_Space) return Wide_String;

   procedure Tail
     (Source : in out Wide_String;
      Count  : Natural;
      Justify : Alignment := Left;
      Pad    : Wide_Character := Ada.Strings.Wide_Space);

   ---------------------------------------
   -- Wide_String Constructor Functions --
   ---------------------------------------

   function "*"
     (Left  : Natural;
      Right : Wide_Character) return Wide_String;

   function "*"
     (Left  : Natural;
      Right : Wide_String) return Wide_String;

end Ada.Strings.Wide_Fixed;
