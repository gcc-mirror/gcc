with Unchecked_Conversion;

package Loop_Optimization18_Pkg is

   type Arr is array (Integer range <>) of Natural;

   type Rec (UB : Integer) is record
      L : Arr (1 .. UB);
   end record;

   type Byte_Array_Type is new String (1..4);

   function Conv is new Unchecked_Conversion (Byte_Array_Type, Integer);

end Loop_Optimization18_Pkg;
