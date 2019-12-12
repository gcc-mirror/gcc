with Task3_Pkg2; use Task3_Pkg2;

package Task3 is
   type Child is new Root with null record;
   type Child_Ptr is access Child;

   type Child_Wrapper is record
      Ptr : Child_Ptr := null;
   end record;

   procedure Destroy (Obj : in out Child_Wrapper);
end Task3;
