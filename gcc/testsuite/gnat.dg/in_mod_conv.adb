-- { dg-do compile }

procedure in_mod_conv is
   package Test is 
     type T  is new Natural range 1..6;
     subtype T_SubType is T range 3..5;
     type A1 is array (T range <>) of boolean;
     type A2 is new A1 (T_SubType);
     PRAGMA pack (A2);
     type New_A2 is new A2; 
  end Test;
  package body Test is 
     procedure P1 (Obj : in New_A2) is
     begin   
        null;   
     end P1; 
     procedure P2 (Data : in out A2) is
     begin   
        P1 (New_A2 (Data (T_SubType)));  -- test 
     end P2; 
  end Test;
begin   
   null;   
end;
