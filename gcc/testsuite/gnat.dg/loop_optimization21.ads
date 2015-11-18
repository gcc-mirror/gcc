package Loop_Optimization21 is

  type Item is new Float;

  type Item_Vector is array (Positive range <>) of Item;

  function Min (X : Item_Vector) return Item;

end Loop_Optimization21;
