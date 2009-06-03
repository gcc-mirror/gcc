package Root is

   type Buffer_Type is array (Positive range <>) of Natural;

   type Root_Type (First : Natural) is abstract tagged record
      Buffer_Root : Buffer_Type (1 .. First);
   end record;

end Root;
