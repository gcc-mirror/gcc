package Lto30 is

   type Rec is private;

   type Ptr is access all Rec;

   procedure Proc;

private

   type Rec is null record;

end Lto30;
