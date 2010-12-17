-- { dg-do compile }

with Static_Initializer5_Pkg; use Static_Initializer5_Pkg;

package Static_Initializer5 is

   type Derived is new Rec with record
      Target : Boolean;
   end record;

   Null_Derived : constant Derived := (Null_Rec with Target => False);

end Static_Initializer5;
