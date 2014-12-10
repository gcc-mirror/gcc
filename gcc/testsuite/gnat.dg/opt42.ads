package Opt42 is

   type Index_Type is range 1 .. 7;
   type Row_Type is array (Index_Type) of Float;
   type Array_Type is array (Index_Type) of Row_Type;

   function "*" (Left, Right : in Array_Type) return Array_Type;

end Opt42;
