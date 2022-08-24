package Hardbool is
   type HBool1 is new Boolean;
   for HBool1'Size use 8;
   for HBool1 use (16#5a#, 16#a5#);
   pragma Machine_Attribute (HBool1, "hardbool");

   type HBool2 is new Boolean;
   for HBool2 use (16#0ff0#, 16#f00f#);
   for HBool2'Size use 16;
   pragma Machine_Attribute (HBool2, "hardbool");

   X : HBool1 := False;
   Y : HBool2 := True;

   function T return Boolean;
   procedure P1;
   procedure P2;
   procedure P3;
   procedure Q1;
   procedure Q2;
   procedure Q3;
end Hardbool;
