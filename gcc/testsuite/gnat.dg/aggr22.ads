with System;

package Aggr22 is

   type Rec is record
     C : Character;
     I : aliased Integer;
   end record;

   My_Rec : aliased Rec;
   pragma Import (Ada, My_Rec);
   for My_Rec'Address use System'To_Address (16#40086000#);

   procedure Proc;

end Aggr22;
