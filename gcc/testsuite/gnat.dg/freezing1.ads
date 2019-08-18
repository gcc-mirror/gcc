with Freezing1_Pack; use Freezing1_Pack;

package Freezing1 is
   type T is abstract tagged record
      Collection : access I_Interface_Collection'Class :=
        new I_Interface_Collection'Class'(Factory.Create_Collection);
   end record;

   procedure Foo;
end Freezing1;
