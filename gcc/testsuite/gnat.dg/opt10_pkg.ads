package Opt10_Pkg is

   type Rep_Message is record
      Bit : Boolean;
      Data : String (1 .. 4);
   end record;
   for Rep_Message use record
      Bit  at 0 range 0 .. 0;
      Data at 0 range 1 .. 32;
   end record;

   procedure Safe_Assign (M : in out Rep_Message; Bit : Boolean);

end;
