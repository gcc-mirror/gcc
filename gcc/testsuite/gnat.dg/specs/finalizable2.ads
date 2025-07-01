-- { dg-do compile }
-- { dg-options "-gnatX0" }

package Finalizable2 is

   type Root is abstract tagged limited null record
      with Finalizable => (Initialize => Initialize);

   procedure Initialize (this : in out Root) is abstract;

   type Ext (L : Natural) is new Root with record
      A : String (1 .. L);
   end record;

   overriding procedure Initialize (this : in out Ext) is null;

   function Make return Ext is (L => 3, A => "asd");

   Obj : Ext := Make;

end Finalizable2;
