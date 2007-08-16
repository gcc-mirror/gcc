--  { dg-do compile }

package body itypes is
   Size : constant := 10;
   type Arr is array (1 .. size) of Integer;
   
   type Rec is record
      Field1 : Arr := (others => 0);
      Field2 : Arr := (others => 0);
      Field3 : Arr := (others => 0);
      Field4 : Arr := (others => 0);
      Field5 : Arr := (others => 0);
      Field6 : Arr := (others => 0);
      Field7 : Arr := (others => 0);
   end record;
   
   procedure Proc is
      Temp1 : Rec;
   begin   
      null;
   end;    
end;                 
