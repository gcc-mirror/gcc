package SPARK2 with SPARK_Mode is

   function Expon (Value, Exp : Natural) return Natural is
      (if Exp = 0 then 1
       else Value * Expon (Value, Exp - 1))
   with Ghost,
        Pre => Value <= Max_Factorial_Number and Exp <= Max_Factorial_Number,
        Annotate => (GNATprove, Terminating);  --  CRASH!

   Max_Factorial_Number : constant := 6;

   function Factorial (N : Natural) return Natural with
      Pre => N < Max_Factorial_Number,
      Post => Factorial'Result <= Expon (Max_Factorial_Number, N);

end SPARK2;
