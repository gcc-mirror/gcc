-- PR tree-optimization/71083
package Loop_Optimization23_Pkg is
  type Nibble is mod 2**4;
  type Int24  is mod 2**24;
  type StructA is record
    a : Nibble;
    b : Int24;
  end record;
  pragma Pack(StructA);
  type StructB is record
    a : Nibble;
    b : StructA;
  end record;
  pragma Pack(StructB);
  type ArrayOfStructB is array(0..100) of StructB;
  procedure Foo (X : in out ArrayOfStructB);
end Loop_Optimization23_Pkg;
