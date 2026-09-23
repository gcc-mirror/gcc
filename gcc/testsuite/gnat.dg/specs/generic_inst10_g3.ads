with Generic_Inst10_G1;
with Generic_Inst10_G2;

generic
   with package Actual_Types is new Generic_Inst10_G1;
package Generic_Inst10_G3 is
   package Actual_Root is new Generic_Inst10_G2;
   package Actual_Holder is new Actual_Root.Holder (Actual_Types);
   package Actual_User is new
     Actual_Root.User (Actual_Types, Actual_Holder);
   package Actual_Final is new
     Actual_Root.Final (Actual_Types, Actual_User);
end Generic_Inst10_G3;
