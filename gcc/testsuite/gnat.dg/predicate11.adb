--  { dg-do compile }
--  { dg-options "-gnata" }

procedure Predicate11 is
   type T_BYTES  is new Integer range 0 .. 2**15 - 1  with Size => 32;
   subtype TYPE5_SCALAR is T_BYTES
     with Dynamic_Predicate => TYPE5_SCALAR mod 4 = 0;
   subtype Cond is Integer
     with dynamic_predicate => (if cond < 5 then false else True);

   Thing1 : Type5_Scalar := 7;  --  { dg-warning "check will fail at run time" }
   function OK (C :Type5_scalar) return Boolean is (True);
   Thing2 : Type5_Scalar;
   Thing3 : Cond;
begin
   if not OK (7) then raise Program_Error; end if;  --  { dg-warning "check will fail at run time" }
   Thing2 := 8;
   Thing3 := 1;  --  { dg-warning "check will fail at run time" }
end;
