--  { dg-do compile }

package body Protected_Func with SPARK_Mode is
   protected body Prot_Obj is
      function Prot_Func return Integer is
      begin
         Comp := Comp + 1;  --  { dg-error "protected function cannot modify protected object" }
         Part_Of_Constit := Part_Of_Constit + 1;  --  { dg-error "protected function cannot modify protected object" }

         return Comp + Part_Of_Constit;
      end Prot_Func;
   end Prot_Obj;
end Protected_Func;
