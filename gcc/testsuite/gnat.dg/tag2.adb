--  { dg-do run }

with Ada.Tags; use Ada.Tags;
with Tag2_Pkg; use Tag2_Pkg;

procedure Tag2 is

   procedure Do_Add_Monitor (Monitor : in out Synchronous_Monitor) is
      Name : constant String :=
        Expanded_Name (Monitor_Interface'Class (Monitor)'Tag);
   begin
      if Name /= "TAG2_PKG.VIRTUAL_INTEGER_REGISTER_REFRESHER" then
         raise Program_Error;
      end if;
   end;

   Obj : Virtual_Integer_Register_Refresher (20);
begin
   Do_Add_Monitor (Synchronous_Monitor (Obj));
end;
