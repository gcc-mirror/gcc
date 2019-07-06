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

--  The language-defined package Strings.Fixed provides string-handling
--  subprograms for fixed-length strings; that is, for values of type
--  Standard.String. Several of these subprograms are procedures that modify
--  the contents of a String that is passed as an out or an in out parameter;
--  each has additional parameters to control the effect when the logical
--  length of the result differs from the parameter's length.
--
--  For each function that returns a String, the lower bound of the returned
--  value is 1.
--
--  The basic model embodied in the package is that a fixed-length string
--  comprises significant characters and possibly padding (with space
--  characters) on either or both ends. When a shorter string is copied to a
--  longer string, padding is inserted, and when a longer string is copied to a
--  shorter one, padding is stripped. The Move procedure in Strings.Fixed,
--  which takes a String as an out parameter, allows the programmer to control
--  these effects. Similar control is provided by the string transformation
--  procedures.

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
   --  The Move procedure copies characters from Source to Target. If Source
   --  has the same length as Target, then the effect is to assign Source to
   --  Target. If Source is shorter than Target then:
   --
   --  * If Justify=Left, then Source is copied into the first Source'Length
   --    characters of Target.
   --
   --  * If Justify=Right, then Source is copied into the last Source'Length
   --    characters of Target.
   --
   --  * If Justify=Center, then Source is copied into the middle Source'Length
   --    characters of Target. In this case, if the difference in length
   --    between Target and Source is odd, then the extra Pad character is on
   --    the right.
   --
   --  * Pad is copied to each Target character not otherwise assigned.
   --
   --  If Source is longer than Target, then the effect is based on Drop.
   --
   --  * If Drop=Left, then the rightmost Target'Length characters of Source
   --    are copied into Target.
   --
   --  * If Drop=Right, then the leftmost Target'Length characters of Source
   --    are copied into Target.
   --
   --  * If Drop=Error, then the effect depends on the value of the Justify
   --    parameter and also on whether any characters in Source other than Pad
   --    would fail to be copied:
   --
   --    * If Justify=Left, and if each of the rightmost
   --      Source'Length-Target'Length characters in Source is Pad, then the
   --      leftmost Target'Length characters of Source are copied to Target.
   --
   --    * If Justify=Right, and if each of the leftmost
   --      Source'Length-Target'Length characters in Source is Pad, then the
   --      rightmost Target'Length characters of Source are copied to Target.
   --
   --    * Otherwise, Length_Error is propagated.

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural;
   pragma Ada_05 (Index);

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;
   pragma Ada_05 (Index);

   --  Each Index function searches, starting from From, for a slice of
   --  Source, with length Pattern'Length, that matches Pattern with respect to
   --  Mapping; the parameter Going indicates the direction of the lookup. If
   --  Source is the null string, Index returns 0; otherwise, if From is not in
   --  Source'Range, then Index_Error is propagated. If Going = Forward, then
   --  Index returns the smallest index I which is greater than or equal to
   --  From such that the slice of Source starting at I matches Pattern. If
   --  Going = Backward, then Index returns the largest index I such that the
   --  slice of Source starting at I matches Pattern and has an upper bound
   --  less than or equal to From. If there is no such slice, then 0 is
   --  returned. If Pattern is the null string, then Pattern_Error is
   --  propagated.

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

   --  If Going = Forward, returns:
   --
   --     Index (Source, Pattern, Source'First, Forward, Mapping)
   --
   --  otherwise, returns:
   --
   --     Index (Source, Pattern, Source'Last, Backward, Mapping).

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural;
   pragma Ada_05 (Index);
   --  Index searches for the first or last occurrence of any of a set of
   --  characters (when Test=Inside), or any of the complement of a set of
   --  characters (when Test=Outside). If Source is the null string, Index
   --  returns 0; otherwise, if From is not in Source'Range, then Index_Error
   --  is propagated. Otherwise, it returns the smallest index I >= From (if
   --  Going=Forward) or the largest index I <= From (if Going=Backward) such
   --  that Source(I) satisfies the Test condition with respect to Set; it
   --  returns 0 if there is no such Character in Source.

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;
   pragma Ada_05 (Index_Non_Blank);
   --  Returns Index (Source, Maps.To_Set(Space), From, Outside, Going)

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural;
   --  Returns Index (Source, Maps.To_Set(Space), Outside, Going)

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural;

   --  Returns the maximum number of nonoverlapping slices of Source that match
   --  Pattern with respect to Mapping. If Pattern is the null string then
   --  Pattern_Error is propagated.

   function Count
     (Source : String;
      Set    : Maps.Character_Set) return Natural;
   --  Returns the number of occurrences in Source of characters that are in
   --  Set.

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);
   pragma Ada_2012 (Find_Token);
   --  If Source is not the null string and From is not in Source'Range, then
   --  Index_Error is raised. Otherwise, First is set to the index of the first
   --  character in Source(From .. Source'Last) that satisfies the Test
   --  condition. Last is set to the largest index such that all characters in
   --  Source(First .. Last) satisfy the Test condition. If no characters in
   --  Source(From .. Source'Last) satisfy the Test condition, First is set to
   --  From, and Last is set to 0.

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);
   --  Equivalent to Find_Token (Source, Set, Source'First, Test, First, Last)

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : String;
      Mapping : Maps.Character_Mapping_Function) return String;

   function Translate
     (Source  : String;
      Mapping : Maps.Character_Mapping) return String;

   --  Returns the string S whose length is Source'Length and such that S (I)
   --  is the character to which Mapping maps the corresponding element of
   --  Source, for I in 1 .. Source'Length.

   procedure Translate
     (Source  : in out String;
      Mapping : Maps.Character_Mapping_Function);

   procedure Translate
     (Source  : in out String;
      Mapping : Maps.Character_Mapping);

   --  Equivalent to Source := Translate(Source, Mapping)

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   procedure Replace_Slice
     (Source  : in out String;
      Low     : Positive;
      High    : Natural;
      By      : String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Character  := Space);
   --  If Low > Source'Last+1, or High < Source'First - 1, then Index_Error is
   --  propagated. Otherwise:
   --
   --  * If High >= Low, then the returned string comprises
   --    Source (Source'First .. Low - 1)
   --    & By & Source(High + 1 .. Source'Last), but with lower bound 1.
   --
   --  * If High < Low, then the returned string is
   --    Insert (Source, Before => Low, New_Item => By).

   function Replace_Slice
     (Source : String;
      Low    : Positive;
      High   : Natural;
      By     : String) return String;
   --  Equivalent to:
   --
   --    Move (Replace_Slice (Source, Low, High, By),
   --          Source, Drop, Justify, Pad).

   function Insert
     (Source   : String;
      Before   : Positive;
      New_Item : String) return String;
   --  Propagates Index_Error if Before is not in
   --  Source'First .. Source'Last+1; otherwise, returns
   --  Source (Source'First .. Before - 1)
   --  & New_Item & Source(Before..Source'Last), but with lower bound 1.

   procedure Insert
     (Source   : in out String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error);
   --  Equivalent to Move (Insert (Source, Before, New_Item), Source, Drop)

   function Overwrite
     (Source   : String;
      Position : Positive;
      New_Item : String) return String;
   --  Propagates Index_Error if Position is not in
   --  Source'First .. Source'Last + 1; otherwise, returns the string obtained
   --  from Source by consecutively replacing characters starting at Position
   --  with corresponding characters from New_Item. If the end of Source is
   --  reached before the characters in New_Item are exhausted, the remaining
   --  characters from New_Item are appended to the string.

   procedure Overwrite
     (Source   : in out String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Right);
   --  Equivalent to Move(Overwrite(Source, Position, New_Item), Source, Drop)

   function Delete
     (Source  : String;
      From    : Positive;
      Through : Natural) return String;
   --  If From <= Through, the returned string is
   --  Replace_Slice(Source, From, Through, ""); otherwise, it is Source with
   --  lower bound 1.

   procedure Delete
     (Source  : in out String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space);
   --  Equivalent to:
   --
   --     Move (Delete (Source, From, Through),
   --           Source, Justify => Justify, Pad => Pad).

   ---------------------------------
   -- String Selector Subprograms --
   ---------------------------------

   function Trim
     (Source : String;
      Side   : Trim_End) return String;
   --  Returns the string obtained by removing from Source all leading Space
   --  characters (if Side = Left), all trailing Space characters (if
   --  Side = Right), or all leading and trailing Space characters (if
   --  Side = Both).

   procedure Trim
     (Source  : in out String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Character := Space);
   --  Equivalent to:
   --
   --     Move (Trim (Source, Side), Source, Justify=>Justify, Pad=>Pad).

   function Trim
     (Source : String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return String;
   --  Returns the string obtained by removing from Source all leading
   --  characters in Left and all trailing characters in Right.

   procedure Trim
     (Source  : in out String;
      Left    : Maps.Character_Set;
      Right   : Maps.Character_Set;
      Justify : Alignment := Strings.Left;
      Pad     : Character := Space);
   --  Equivalent to:
   --
   --     Move (Trim (Source, Left, Right),
   --           Source, Justify => Justify, Pad=>Pad).

   function Head
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String;
   --  Returns a string of length Count. If Count <= Source'Length, the string
   --  comprises the first Count characters of Source. Otherwise, its contents
   --  are Source concatenated with Count - Source'Length Pad characters.

   procedure Head
     (Source  : in out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space);
   --  Equivalent to:
   --
   --     Move (Head (Source, Count, Pad),
   --           Source, Drop => Error, Justify => Justify, Pad => Pad).

   function Tail
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String;
   --  Returns a string of length Count. If Count <= Source'Length, the string
   --  comprises the last Count characters of Source. Otherwise, its contents
   --  are Count-Source'Length Pad characters concatenated with Source.

   procedure Tail
     (Source  : in out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space);
   --  Equivalent to:
   --
   --     Move (Tail (Source, Count, Pad),
   --           Source, Drop => Error, Justify => Justify, Pad => Pad).

   ----------------------------------
   -- String Constructor Functions --
   ----------------------------------

   function "*"
     (Left  : Natural;
      Right : Character) return String;

   function "*"
     (Left  : Natural;
      Right : String) return String;

   --  These functions replicate a character or string a specified number of
   --  times. The first function returns a string whose length is Left and each
   --  of whose elements is Right. The second function returns a string whose
   --  length is Left * Right'Length and whose value is the null string if
   --  Left = 0 and otherwise is (Left - 1)*Right & Right with lower bound 1.

end Ada.Strings.Fixed;
