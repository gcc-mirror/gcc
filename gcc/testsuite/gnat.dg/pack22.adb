-- { dg-do compile }
-- { dg-options "-O -gnatws" }

with Pack22_Pkg; use Pack22_Pkg;

procedure Pack22 is

   package Role_Map is new Bit_Map_Generic;

   type Role_List is new Role_Map.List;
   Roles_1 : Role_List;
   Roles_2 : Role_List;
   Roles_3 : Role_List;

begin
   Temp_buffer := (others => 1);
   Temp_Buffer(2) := (0);
   Roles_1 := Roles_2 xor Roles_3;
end;
