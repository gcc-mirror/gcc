with Ada.Finalization;

package Access8_Pkg is

   type Discriminant is record
      Component : Integer := 6;
   end record;

   type Object (D : access Discriminant)
     is tagged limited private;

   function Get return Object;
   function Get_Access return access Object;
private
   type Object (D : access Discriminant)
    is new Ada.Finalization.Limited_Controlled with null record;
   overriding procedure Initialize (O : in out Object);
   overriding procedure Finalize (O : in out Object);
end;
