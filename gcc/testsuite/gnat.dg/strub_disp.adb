--  { dg-do compile }
--  { dg-require-effective-target strub }

procedure Strub_Disp is
   package Foo is
      type A is tagged null record;

      procedure P (I : Integer; X : A);
      pragma Machine_Attribute (P, "strub", "at-calls");
      
      function F (X : access A) return Integer;

      type B is new A with null record;
      
      overriding
      procedure P (I : Integer; X : B); -- { dg-error "requires the same .strub. mode" }

      overriding
      function F (X : access B) return Integer;
      pragma Machine_Attribute (F, "strub", "at-calls"); -- { dg-error "requires the same .strub. mode" }

   end Foo;

   package body Foo is
      procedure P (I : Integer; X : A) is
      begin
	 null;
      end;
      
      function F (X : access A) return Integer is (0);

      overriding
      procedure P (I : Integer; X : B) is
      begin
	 P (I, A (X));
      end;
      
      overriding
      function F (X : access B) return Integer is (1);
   end Foo;

   use Foo;

   procedure Q (X : A'Class) is
   begin
      P (-1, X);
   end;

   XA : aliased A;
   XB : aliased B;
   I : Integer := 0;
   XC : access A'Class;
begin
   Q (XA);
   Q (XB);
   
   I := I + F (XA'Access);
   I := I + F (XB'Access);

   XC := XA'Access;
   I := I + F (XC);

   XC := XB'Access;
   I := I + F (XC);
end Strub_Disp;
