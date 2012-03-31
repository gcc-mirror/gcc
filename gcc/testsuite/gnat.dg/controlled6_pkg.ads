with Ada.Finalization;

generic

   type T is private;

package Controlled6_Pkg is

   type Node_Type is record
      Item : T;
   end record;

   type Node_Access_Type is access Node_Type;

end Controlled6_Pkg;
