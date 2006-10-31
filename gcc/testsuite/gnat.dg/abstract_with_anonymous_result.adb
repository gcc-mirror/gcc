-- { dg-do run }

procedure Abstract_With_Anonymous_Result is

   package Pkg is
      type I is abstract tagged null record;
      type Acc_I_Class is access all I'Class;
      function Func (V : I) return access I'Class is abstract;
      procedure Proc (V : access I'Class);
      type New_I is new I with null record;
      function Func (V : New_I) return access I'Class;
   end Pkg;

   package body Pkg is
      X : aliased New_I;

      procedure Proc (V : access I'Class) is begin null; end Proc;

      function Func (V : New_I) return access I'Class is
      begin
         X := V;
         return X'Access;
      end Func;
   end Pkg;

   use Pkg;

   New_I_Obj : aliased New_I;

   procedure Proc2 (V : access I'Class) is
   begin
      Proc (Func (V.all));  -- Call to Func causes gigi abort 122
   end Proc2;

begin
   Proc2 (New_I_Obj'Access);
end Abstract_With_Anonymous_Result;
