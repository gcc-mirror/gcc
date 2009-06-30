-- { dg-do compile }

procedure Discr19 is

   type Arr_Int_T is array (Integer range <>) of Integer;

   type Abs_Tag_Rec_T (N : Integer; M : Integer) is abstract tagged record
      Arr_Int : Arr_Int_T (1..M);
   end record;

   type Tag_Rec_T (M : Integer)
     is new Abs_Tag_Rec_T (N => 1, M => M) with null record;

begin
   null;
end;
