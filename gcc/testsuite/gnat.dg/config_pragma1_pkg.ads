pragma Assertion_Policy (Ignore);

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Hashed_Maps;
with Ada.Strings;    use Ada.Strings;
with Ada.Strings.Hash;

package Config_Pragma1_Pkg is
   subtype Positive10 is Positive range 1 .. 1000;
   subtype String10 is String (Positive10);

   package FHM is new Formal_Hashed_Maps
     (Key_Type        => String10,
      Element_Type    => Positive10,
      Hash            => Hash,
      Equivalent_Keys => "=");

   FHMM : FHM.Map
     (Capacity => 1_000_000,
      Modulus  => FHM.Default_Modulus (Count_Type (1_000_000)));
end Config_Pragma1_Pkg;
