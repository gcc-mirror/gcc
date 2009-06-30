-- { dg-do compile }

with Discr18_Pkg; use Discr18_Pkg;

procedure Discr18 is

  String_10 : String (1..10) := "1234567890";

  MD : Multiple_Discriminants (A => 10, B => 10) :=
         Multiple_Discriminants'(A  => 10,
                                 B  => 10,
                                 S1 => String_10,
                                 S2 => String_10);
  MDE : Multiple_Discriminant_Extension (C => 10) :=
          (MD with C  => 10, S3 => String_10);

begin
  Do_Something(MDE);
end;
