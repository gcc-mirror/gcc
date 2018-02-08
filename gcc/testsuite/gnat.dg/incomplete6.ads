with Ada.Unchecked_Conversion;

package Incomplete6 is
   
   type Vint;
   function "=" (Left, Right : Vint) return Boolean;

   type Vint is record
      Value : Integer;
   end record;

   function To_Integer is new 
     Ada.Unchecked_Conversion(Source => Vint, Target => Integer);
   
   type Vfloat;
   function "=" (Left, Right : in Vfloat) return Boolean;

   type Vfloat is record
      Value : Float;
   end record;

end;
