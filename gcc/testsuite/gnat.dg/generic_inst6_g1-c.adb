with Generic_Inst6_X;
package body Generic_Inst6_G1.C is
   package N is new Generic_Inst6_X
      (Generic_Inst6_G1, Generic_Inst6_G1);
   function Check return Integer is (N.Result);
end;
