-- { dg-do run }
-- { dg-options "-gnatws" }

procedure Test_Bounded is
   type Bounded (Length : Natural := 0) is
      record
         S : String (1..Length);
      end record;
   type Ref is access all Bounded;
   X : Ref := new Bounded;
begin
   null;
end Test_Bounded;
