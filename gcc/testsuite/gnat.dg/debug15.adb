-- { dg-do compile }
-- { dg-options "-g1" }

procedure Debug15 is

   type Shape is abstract tagged record
      S : Integer;
   end record;

   type Rectangle is new Shape with record
      R : Integer;
   end record;

   X : Integer;

   R: Rectangle := (1, 2);
   S: Shape'Class := R;

begin
   X := 12;
end;

-- { dg-final { scan-assembler-not "loc 2" } }
