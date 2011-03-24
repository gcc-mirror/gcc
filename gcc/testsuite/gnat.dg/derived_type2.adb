-- { dg-do run }
-- { dg-options "-gnatws" }

procedure Derived_Type2 is

   package Pkg is

      type Parent (B : Boolean := True) is record
         case B is
            when True => S : String (1 .. 5);
            when False => F : Float;
         end case;
      end record;

      function Create (X : Parent) return Parent;

   end Pkg;

   package body Pkg is

      function Create (X : Parent) return Parent is
      begin
         return (True, "12345");
      end;

   end Pkg;

   use Pkg;

   type T is new Parent (True);

   X : T;

begin

   if Create (X).B /= True then
      raise Program_Error;
   end if;

end;
