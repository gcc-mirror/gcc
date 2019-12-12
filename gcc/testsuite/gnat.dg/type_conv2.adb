--  { dg-do compile }

package body Type_Conv2 is

   function Wrap (X : Integer) return Root'Class is
   begin
      return Der_I'(X => X);
   end Wrap;

   procedure Proc_Static is
      D : constant Der_I := Der_I (Wrap (0));  --  { dg-error "initialization of limited object requires aggregate or function call" }
   begin
      null;
   end Proc_Static;

end Type_Conv2;
