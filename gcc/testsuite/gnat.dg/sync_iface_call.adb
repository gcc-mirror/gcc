--  { dg-do compile }

with Sync_Iface_Call_Pkg;
with Sync_Iface_Call_Pkg2;

procedure Sync_Iface_Call is

   Impl : access Sync_Iface_Call_Pkg.IFace'Class :=
       new Sync_Iface_Call_Pkg2.Impl;
   Val : aliased Integer := 10;
begin
   select
      Impl.Do_Stuff (Val);
   or
      delay 10.0;
   end select;
   select
      Impl.Do_Stuff_Access (Val'Access);
   or
      delay 10.0;
   end select;

   select
      Impl.Do_Stuff_2 (Val);
   or
      delay 10.0;
   end select;

   select
      Impl.Do_Stuff_2_Access (Val'Access);
   or
      delay 10.0;
   end select;
end Sync_Iface_Call;
