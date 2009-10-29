-- { dg-do compile }

procedure Pack14 is

   subtype False_T is Boolean range False .. False;

   type Rec is record
      F : False_T;
   end record;
   pragma Pack (Rec);

   A : Rec := (F => False);

begin
   null;
end;
