with Limited2_Pack_1;

package body Limited2_Pack_2 is
   Obj_1 : Limited2_Pack_1.A;
   Obj_2 : Limited2_Pack_1.A;
   Obj_3 : Limited2_Pack_1.A;

   procedure M (R : Limited2_Pack_1.A) is
   begin
      null;
   end M;

   procedure Create (P : in C) is
   begin
      M (R => Obj_1);
      M (R => (case P is
                 when C1 => Obj_1,
                 when C2 => Obj_2,
                 when C3 => Obj_3));
   end Create;
end Limited2_Pack_2;
