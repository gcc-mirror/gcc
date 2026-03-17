-- PR ada/124201
-- { dg-do compile }
-- { dg-options "-gnat2022" }

with Ada.Containers.Indefinite_Ordered_Maps;

procedure Generic_Inst17 is

   package Nested is
      type Axis_Name is (X_Axis, Y_Axis, Z_Axis, E_Axis);

      package Status_Group_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps (String, String);

      generic
      package Modules is
         type Module is abstract tagged null record;
         function Status_Schema (This : Module) return Status_Group_Maps.Map
         is ([]);
      end Modules;

      generic
         with package My_Modules is new Modules;
      package Internal_Status_Reporter is
         type Module is new My_Modules.Module with null record;
         function Status_Schema (This : Module) return Status_Group_Maps.Map
         is ([for A in Axis_Name use A'Image => ""]);
      end Internal_Status_Reporter;

      generic
      package Controller is
         package My_Modules is new Modules;
         package My_Internal_Status_Reporter is new
           Internal_Status_Reporter (My_Modules);
      end Controller;
   end Nested;

   package My_Controller is new Nested.Controller;

begin
  null;
end Generic_Inst17;
