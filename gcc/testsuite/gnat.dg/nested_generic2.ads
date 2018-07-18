with Nested_Generic2_G1;
with Nested_Generic2_G2;

package Nested_Generic2 is

   package My_G1 is new Nested_Generic2_G1 ("Lewis");
   package My_G2 is new Nested_Generic2_G2 (T => Integer, P => My_G1);

   procedure Dummy;

private
   package My_Nested is new My_G1.Nested ("Clark");
end Nested_Generic2;
