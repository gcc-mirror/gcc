with Generic_Inst6_G1;
generic
   with package G2 is new Generic_Inst6_G1 (<>);
   with package G3 is new Generic_Inst6_G1 (<>);
package Generic_Inst6_X is
   Result : Integer := G2.Val * G3.Val;
end;
