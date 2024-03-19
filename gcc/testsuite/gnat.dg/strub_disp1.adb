--  { dg-do compile }
--  { dg-options "-fdump-ipa-strub" }
--  { dg-require-effective-target strub }

-- Check that at-calls dispatching calls are transformed.

procedure Strub_Disp1 is
   package Foo is
      type A is tagged null record;

      procedure P (I : Integer; X : A);
      pragma Machine_Attribute (P, "strub", "at-calls");
      
      function F (X : access A) return Integer;
      pragma Machine_Attribute (F, "strub", "at-calls");

      type B is new A with null record;
      
      overriding
      procedure P (I : Integer; X : B);
      pragma Machine_Attribute (P, "strub", "at-calls");

      overriding
      function F (X : access B) return Integer;
      pragma Machine_Attribute (F, "strub", "at-calls");

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
	 P (I, A (X)); -- strub-at-calls non-dispatching call
      end;
      
      overriding
      function F (X : access B) return Integer is (1);
   end Foo;

   use Foo;

   procedure Q (X : A'Class) is
   begin
      P (-1, X); -- strub-at-calls dispatching call.
   end;

   XA : aliased A;
   XB : aliased B;
   I : Integer := 0;
   XC : access A'Class;
begin
   Q (XA);
   Q (XB);
   
   I := I + F (XA'Access); -- strub-at-calls non-dispatching call
   I := I + F (XB'Access); -- strub-at-calls non-dispatching call

   XC := XA'Access;
   I := I + F (XC); -- strub-at-calls dispatching call.

   XC := XB'Access;
   I := I + F (XC); -- strub-at-calls dispatching call.
end Strub_Disp1;

--  { dg-final { scan-ipa-dump-times "\[(\]strub \[(\]at-calls\[)\]\[)\]" 4 "strub" } }

--  Count the strub-at-calls non-dispatching calls 
--  (+ 2 each, for the matching prototypes)
--  { dg-final { scan-ipa-dump-times "foo\.p \[(\]\[^\n\]*watermark" 3 "strub" } }
--  { dg-final { scan-ipa-dump-times "foo\.f \[(\]\[^\n\]*watermark" 4 "strub" } }

--  Count the strub-at-calls dispatching calls.
--  { dg-final { scan-ipa-dump-times "_\[0-9\]* \[(\]\[^\n\]*watermark" 3 "strub" } }
