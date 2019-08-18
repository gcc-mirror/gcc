generic
   type Real is digits <>;
package Interface9_Root is
   type Base_Interface is limited interface;

   procedure Primitive1 (B : in out Base_Interface) is abstract;
   procedure Primitive2 (B : in out Base_Interface) is null;

   type Derived_Interface is limited interface and Base_Interface;
end Interface9_Root;
