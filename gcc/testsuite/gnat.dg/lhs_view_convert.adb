-- { dg-do run }
-- { dg-options "-gnatp" }

procedure Lhs_View_Convert is

   type Root is tagged record
      RV : Natural;
   end record;

   type Derived is new Root with null record;

   Root_Instance : Root := (RV => 1);

   Derived_Instance : Derived;

   procedure Process is
      X : Natural := Derived_Instance.RV;
   begin
      null;
   end;
begin
   Derived_Instance.RV := 2;
   
   Root (Derived_Instance) := Root (Root_Instance);
   
   if Derived_Instance.RV /= Root_Instance.RV then
      raise Program_Error;
   end if;
end;
