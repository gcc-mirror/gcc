--  { dg-do run }
--  { dg-options "-gnatws" }

procedure Tfren is
   type R;
   type Ar is access all R;
   type R is record F1: Integer; F2: Ar; end record;
   
   for R use record
      F1 at 1 range 0..31;
     F2 at 5 range 0..63;
   end record;                                                

   procedure Foo (RR1, RR2: Ar);

   procedure Foo (RR1, RR2 : Ar) is
   begin
      if RR2.all.F1 /= 55 then raise program_error; end if;
   end;

   R3: aliased R := (55, Null);
   R2: aliased R := (44, R3'Access);
   R1: aliased R := (22, R2'Access);
   P: Ar := R1'Access;

   X: Ar renames P.all.F2;
   Y: Ar renames X.all.F2;

begin
   P := R2'Access;
   R1.F2 := R1'Access;
   Foo (X, Y);
   Y.F1 := -111;
   if Y.F1 /= -111 then raise Constraint_Error; end if;
end Tfren;
