package Protected_Func with SPARK_Mode is
   protected Prot_Obj is
      function Prot_Func return Integer;
   private
      Comp : Integer := 0;
   end Prot_Obj;

   Part_Of_Constit : Integer := 0 with Part_Of => Prot_Obj;
end Protected_Func;
