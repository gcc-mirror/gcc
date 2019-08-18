package Range_Check3_Pkg is
   type Array_Type is array (Positive range <>) of Integer;
   type Array_Access is access Array_Type;

   function One  return Positive;
   function Zero return Natural;

   function Allocate return Array_Access;
end Range_Check3_Pkg;
