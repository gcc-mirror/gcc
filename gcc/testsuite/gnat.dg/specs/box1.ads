--  { dg-do compile }

package box1 is
  type Root is tagged null record;
  type Der1 is new Root with record
     B : Boolean;
  end record;
  
  type Der2 is new Der1 with null record;
  type Der3 is new Der2 with null record;
  
  Obj : Der3 := (Der2 with others => <>);
end;
