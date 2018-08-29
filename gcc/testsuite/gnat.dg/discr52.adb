--  { dg-do compile }

procedure Discr52 is
   type T_Root (Root_Disc : Natural) is record
      Data : Natural := 0;
   end record;

   type T_Derived (deriv_disc : Natural) is
     new T_Root (root_disc => deriv_disc);

   Derived : T_Derived (Deriv_Disc => 3);
   Value   : Natural;

   procedure Do_Test (Obj : T_Derived) is
   begin
      Value := Obj.root_disc; --  { dg-error "no selector \"root_disc\" for type \"T_Derived\" defined at line \\d+" }
   end;
begin
   Do_Test (Derived);
end;
