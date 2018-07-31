package body Part_Of1.Private_Generic
with
   Refined_State => (State => Numbers)
is
   Numbers : array (Range_Type) of Integer := (others => 0);

   function Get (I : Range_Type) return Integer
   is
   begin
      return Numbers (I);
   end Get;

end Part_Of1.Private_Generic;
