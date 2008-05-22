--  { dg-do compile }

package CPP_Assignment is
   type T is tagged record
      Data : Integer := 0;
   end record;
   pragma Convention (CPP, T); 

   Obj1 : T := (Data => 1);                                                        Obj2 : T'Class := Obj1;
end;
