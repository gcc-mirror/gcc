--  { dg-do compile }
--  { dg-require-effective-target strub }

--  Check that strub mode mismatches between overrider and overridden
--  subprograms are reported even when the overriders for an
--  interface's subprograms are inherited from a type that is not a
--  descendent of the interface.

procedure Strub_Intf2 is
   package Foo is
      type A is tagged null record;

      procedure P (I : Integer; X : A);
      pragma Machine_Attribute (P, "strub", "at-calls"); -- { dg-error "requires the same .strub. mode" }
      
      function F (X : access A) return Integer;

      type TX is Interface;

      procedure P (I : Integer; X : TX) is abstract; 

      function F (X : access TX) return Integer is abstract;
      pragma Machine_Attribute (F, "strub", "at-calls");

      type B is new A and TX with null record; -- { dg-error "requires the same .strub. mode" }

   end Foo;

   package body Foo is
      procedure P (I : Integer; X : A) is
      begin
	 null;
      end;
      
      function F (X : access A) return Integer is (0);

   end Foo;

   use Foo;

   procedure Q (X : TX'Class) is
   begin
      P (-1, X);
   end;

   XB : aliased B;
   I : Integer := 0;
   XC : access TX'Class;
begin
   Q (XB);
   
   I := I + F (XB'Access);

   XC := XB'Access;
   I := I + F (XC);
end Strub_Intf2;
