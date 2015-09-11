generic

   type T is private;

package Varsize3_Pkg3 is

   type Object is record
      E : T;
   end record;

   function True return Object;

end Varsize3_Pkg3;
