package Opt78 is

   subtype Reasonable is Integer range 1..10;

   type UC (D: Reasonable := 2) is record
      S: String (1 .. D) := "Hi";
   end record;

   type AUC is access all UC;

   procedure Proc (P : UC; Msg : String);

end Opt78;
