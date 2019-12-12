with Ada.Strings.Bounded;

package Derived_Type6 is
  package b is new Ada.Strings.Bounded.Generic_Bounded_Length(10);
  subtype s1 is b.Bounded_String;
  type s2 is new s1;

  procedure Foo;
end Derived_Type6;
