--  { dg-do compile }

with text_io;
with System;
procedure addr3 is
  
  Type T_SAME_TYPE is new System.Address;
  
  Type T_OTHER_TYPE is new System.Address;
  
  I : constant integer := 0;                                                   
                                                                                  procedure dum ( i : INTEGER ) is
  begin
    text_io.put_line ("Integer op");
    null;
  end;
  
  procedure dum ( i : system.ADDRESS ) is
  begin
    null;
  end;
  
  procedure dum ( i : T_SAME_TYPE ) is
  begin
    null;
  end;
  
  procedure dum ( i : T_OTHER_TYPE ) is
  begin
    null;
  end;

begin
   dum( I );
   dum( 1 );
end; 
