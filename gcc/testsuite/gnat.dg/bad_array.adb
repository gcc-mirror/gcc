--  { dg-do compile }

procedure Bad_Array is
   A1 : array(Character range <> ) of Character := ( 'a', 'b', 'c' );
begin
   null;
end Bad_Array;
