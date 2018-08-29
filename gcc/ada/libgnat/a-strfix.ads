------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . S T R I N G S . F I X E D                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Maps;

package Ada.Strings.Fixed is
   pragma Preelaborate;

   --------------------------------------------------------------
   -- Copy Procedure for Strings of Possibly Different Lengths --
   --------------------------------------------------------------

   procedure Move
     (Source  : String;
      Target  : out String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Character  := Space);

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural;

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural;
   pragma Ada_05 (Index);

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural;

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;
   pragma Ada_05 (Index_Non_Blank);

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural;

   function Count
     (Source : String;
      Set    : Maps.Character_Set) return Natural;

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);
   pragma Ada_2012 (Find_Token);

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : String;
      Mapping : Maps.Character_Mapping) return String;

   procedure Translate
     (Source  : in out String;
      Mapping : Maps.Character_Mapping);

   function Translate
     (Source  : String;
      Mapping : Maps.Character_Mapping_Function) return String;

   procedure Translate
     (Source  : in out String;
      Mapping : Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : String;
      Low    : Positive;
      High   : Natural;
      By     : String) return String;

   procedure Replace_Slice
     (Source  : in out String;
      Low     : Positive;
      High    : Natural;
      By      : String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Character  := Space);

   function Insert
     (Source   : String;
      Before   : Positive;
      New_Item : String) return String;

   procedure Insert
     (Source   : in out String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error);

   function Overwrite
     (Source   : String;
      Position : Positive;
      New_Item : String) return String;

   procedure Overwrite
     (Source   : in out String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Right);

   function Delete
     (Source  : String;
      From    : Positive;
      Through : Natural) return String;

   procedure Delete
     (Source  : in out String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space);

   ---------------------------------
   -- String Selector Subprograms --
   ---------------------------------

   function Trim
     (Source : String;
      Side   : Trim_End) return String;

   procedure Trim
     (Source  : in out String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Character := Space);

   function Trim
     (Source : String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return String;

   procedure Trim
     (Source  : in out String;
      Left    : Maps.Character_Set;
      Right   : Maps.Character_Set;
      Justify : Alignment := Strings.Left;
      Pad     : Character := Space);

   function Head
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String;

   procedure Head
     (Source  : in out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space);

   function Tail
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String;

   procedure Tail
     (Source  : in out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space);

   ----------------------------------
   -- String Constructor Functions --
   ----------------------------------

   function "*"
     (Left  : Natural;
      Right : Character) return String;

   function "*"
     (Left  : Natural;
      Right : String) return String;

end Ada.Strings.Fixed;
