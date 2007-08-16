package deref1 is
  type T is tagged limited null record;
   procedure Op (Obj : in out T);
end deref1;
