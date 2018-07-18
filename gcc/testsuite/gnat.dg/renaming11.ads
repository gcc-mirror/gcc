package Renaming11 is

   subtype Index_Type is Integer range 1..10;

   type Arr is array (Index_Type range <>) of Integer;

   type Rec (Min : Index_Type; Max : Index_Type) is record
      A : Arr (Min .. Max);
   end record;

   type Ptr1 is access Rec;

   type Ptr2 is access Ptr1;

   type Ptr3 is access Ptr2;

   function F (Arg : Ptr3) return Integer;

end Renaming11;
