
package body access3 is
   
   type IT_Access is not null access all IT'Class;
   for IT_Access'Storage_Size use 0;
   
   procedure Op
     (Obj_T2 : in out T2;
      Obj_IT : not null access IT'Class)
   is 
      X : constant IT_Access := Obj_IT.all'Unchecked_Access;
   begin
      null;
   end Op;

end access3;
