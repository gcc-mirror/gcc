--  { dg-do compile }
--  { dg-require-effective-target strub }

--  Check that strub mode mismatches between overrider and overridden
--  subprograms are reported.

procedure Strub_Intf is
   package Foo is
      type TP is interface;
      procedure P (I : Integer; X : TP) is abstract;
      pragma Machine_Attribute (P, "strub", "at-calls"); -- { dg-error "requires the same .strub. mode" }

      type TF is interface;
      function F (X : access TF) return Integer is abstract;

      type TX is interface;
      procedure P (I : Integer; X : TX) is abstract;

      type TI is interface and TP and TF and TX;
      --  When we freeze TI, we detect the mismatch between the
      --  inherited P and another parent's P.  Because TP appears
      --  before TX, we inherit P from TP, and report the mismatch at
      --  the pragma inherited from TP against TX's P.  In contrast,
      --  when we freeze TII below, since TX appears before TP, we
      --  report the error at the line in which the inherited
      --  subprogram is synthesized, namely the line below, against
      --  the line of the pragma.

      type TII is interface and TX and TP and TF; -- { dg-error "requires the same .strub. mode" }

      function F (X : access TI) return Integer is abstract;
      pragma Machine_Attribute (F, "strub", "at-calls"); -- { dg-error "requires the same .strub. mode" }

      type A is new TI with null record;

      procedure P (I : Integer; X : A);
      pragma Machine_Attribute (P, "strub", "at-calls"); -- { dg-error "requires the same .strub. mode" }
      
      function F (X : access A) return Integer; -- { dg-error "requires the same .strub. mode" }

      type B is new TI with null record;
      
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
	 null;
      end;
      
      overriding
      function F (X : access B) return Integer is (1);

   end Foo;

   use Foo;

   procedure Q (X : TX'Class) is
   begin
      P (-1, X);
   end;

   XA : aliased A;
   XB : aliased B;
   I : Integer := 0;
   XC : access TI'Class;
begin
   Q (XA);
   Q (XB);
   
   I := I + F (XA'Access);
   I := I + F (XB'Access);

   XC := XA'Access;
   I := I + F (XC);

   XC := XB'Access;
   I := I + F (XC);
end Strub_Intf;
