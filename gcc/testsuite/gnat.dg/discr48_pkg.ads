with Ada.Finalization;

package Discr48_Pkg is

   type XString is new Ada.Finalization.Controlled with record
      B : Boolean;
   end record;

   Null_XString : constant XString := (Ada.Finalization.Controlled with B => False);

   type XString_Array is array (Natural range <>) of XString;

   type Rec (Count : Positive) is record
      Seps : XString_Array (2 .. Count);
   end record;

   type Rec_Access is access all Rec;

end Discr48_Pkg;
