package Assertion_Policy1_Pkg is
   pragma Assertion_Policy (Ignore);

   procedure Proc (Low : Integer; High : Integer)
     with Pre => Low < High;
end Assertion_Policy1_Pkg;
