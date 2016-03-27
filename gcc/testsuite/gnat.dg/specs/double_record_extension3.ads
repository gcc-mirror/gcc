-- { dg-do compile }

package Double_Record_Extension3 is

   type Rec1 is tagged record
      Id : Integer;
   end record;

   for Rec1 use record
      Id at 8 range 0 .. 31;
   end record;

   type Rec2 (Size : Integer) is new Rec1 with record
      Data : String (1 .. Size);
   end record;

   type Rec3 is new Rec2 (Size => 128) with record
      Valid : Boolean;
   end record;

end Double_Record_Extension3;

