--  { dg-do compile }
--  { dg-options "-gnatws" }

package body array1 is
  
  subtype Small is Integer range 1 .. MAX;
  
  type LFT is record
    RIC_ID : RIC_TYPE;
  end record;
  
  LF : array (RIC_TYPE, Small) of LFT;
  
  procedure Foo (R : RIC_TYPE) is
    L : Small;
    T : LFT renames LF (R, L);
  begin
    Start_Timer (T'ADDRESS);
  end;
  
  procedure Bar (A : Integer; R : RIC_TYPE) is
    S : LFT renames LF (R, A);
  begin
    null;
  end;
  
  procedure Start_Timer (Q : SYSTEM.ADDRESS) is
  begin                                                        
    null;                                                      
  end;

end array1;
