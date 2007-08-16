
package access3 is
   type IT is limited interface;
   type T is limited new IT with null record;
   
   type T2 is tagged limited null record;
   
   procedure Op
     (Obj_T2 : in out T2;
      Obj_IT : not null access IT'Class);
end access3;
