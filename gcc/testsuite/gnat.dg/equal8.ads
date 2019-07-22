with Ada.Containers.Formal_Hashed_Sets;
with Ada.Strings.Hash;

-- with Dynamic_Strings; use Dynamic_Strings;
-- with Bounded_Dynamic_Strings;

with Equal8_Pkg;

package Equal8 is

   package Dynamic_Strings is
      --  pragma SPARK_Mode (On);

      package Bounded_Dynamic_Strings is new Equal8_Pkg
  (Component     => Character,
   List_Index    => Positive,
   List          => String,
   Default_Value => ' ');
      type Dynamic_String is new Bounded_Dynamic_Strings.Sequence;

   end Dynamic_Strings;
   use Dynamic_Strings;

   subtype Subscription_Address is Dynamic_String (Capacity => 255);

   function Hashed_Subscription_Address (Element : Subscription_Address)
      return Ada.Containers.Hash_Type is
      (Ada.Strings.Hash (Value (Element)));

   package Subscription_Addresses is new Ada.Containers.Formal_Hashed_Sets
     (Element_Type        => Subscription_Address,
      Hash                => Hashed_Subscription_Address,
      Equivalent_Elements => "=");

   procedure Foo;
end Equal8;
