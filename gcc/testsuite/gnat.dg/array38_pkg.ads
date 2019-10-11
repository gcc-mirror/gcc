package Array38_Pkg is

   type Byte is mod 2**8;

   type Length is new Natural;
   subtype Index is Length range 1 .. Length'Last;

   type Bytes is array (Index range <>) of Byte with
     Predicate => Bytes'Length > 0;

   generic
      type Index_Type   is (<>);
      type Element_Type is (<>);
      type Array_Type   is array (Index_Type range <>) of Element_Type;
      type Value_Type   is (<>);
   function F (Data : Array_Type) return Value_Type;

end Array38_Pkg;
