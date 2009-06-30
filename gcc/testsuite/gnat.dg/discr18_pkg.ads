package Discr18_Pkg is

   subtype Length is Natural range 0..256;

   type Multiple_Discriminants (A, B : Length) is tagged
      record
         S1 : String (1..A);
         S2 : String (1..B);
      end record;

   procedure Do_Something (Rec : in out Multiple_Discriminants);

   type Multiple_Discriminant_Extension (C : Length) is
      new Multiple_Discriminants (A => C, B => C)
      with record
         S3 : String (1..C);
      end record;

end Discr18_Pkg;
