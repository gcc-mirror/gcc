with Ada.Tags;

package body Controlled5_Pkg is

   type Child is new Root with null record;

   function Dummy (I : Integer) return Root'Class is
      A1 : T_Root_Class := new Child;
      My_Var : Root'Class := A1.all;
   begin
      if I = 0 then
         return My_Var;
      else
         return Dummy (I - 1);
      end if;
   end Dummy;

end Controlled5_Pkg;
