with Ada.Finalization; use Ada.Finalization;
package controlled3 is
   type Test is new Controlled with null record;
   procedure Add_Test (T : access Test'Class);
   
   type Test_Case1 is new Test with null record;
   type Test_Suite is new Test with null record;
   
   type Test_Case is new Test_Case1 with record
      Link_Under_Test : Natural;
   end record;
end;
