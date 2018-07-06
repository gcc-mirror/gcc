with Nested_Generic2_G1;

generic
  type T is private;
  with package P is new Nested_Generic2_G1 (<>);
package Nested_Generic2_G2 is
end Nested_Generic2_G2;
