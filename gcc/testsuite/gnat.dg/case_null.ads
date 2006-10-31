package Case_Null is
   type T is (a, b, c, d, e);

   subtype S is T range b .. d;

   subtype S1 is S range a .. d;
   --  Low bound out of range of base subtype.

   procedure P1 (X : T);

end Case_Null;
