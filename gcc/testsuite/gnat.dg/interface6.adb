--  { dg-do compile }

procedure Interface6 is

     type TI  is interface;
     type TI2 is interface;

     type Rec_Type is tagged null record;

     type Rec_Type1 is new TI
     with
     record
         A : Integer;
     end record;

     type Rec_Type2 is new Rec_Type1 and TI2
     with
     record
         B : Integer;
     end record;

     type Rec_Type12 is new Rec_Type1 and TI and TI2
     with
     record
         C : Integer;
     end record;

     generic
         type T is new Rec_Type1 and TI2 with private;
     procedure Test;

     procedure Test is
     begin
         null;
     end Test;

     procedure Test_Instance1 is new Test (T => Rec_Type);  --  { dg-error "actual must implement all interfaces of formal \"T\"" }
     procedure Test_Instance1 is new Test (T => Rec_Type1);  -- { dg-error "Actual \"Rec_Type1\" must implement interface \"TI2\"" }
     procedure Test_Instance2 is new Test (T => Rec_Type2);
     procedure Test_Instance12 is new Test (T => Rec_Type12);

begin
     null;
end Interface6;
