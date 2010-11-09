package Static_Initializer5_Pkg is

   type Arr is array (Positive range <>) of Character;

   type Buffer_Type (Length : Positive) is record
      Content : Arr (1 .. Length);
   end record;

   type Buffer_Access is access Buffer_Type;

   type Rec is tagged record
      Buffer : Buffer_Access;
   end record;

   Null_Rec : constant Rec := (Buffer => null);

end Static_Initializer5_Pkg;
