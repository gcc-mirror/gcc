--  { dg-do compile }
--  { dg-options "-gnatws" }

procedure Addr_Slice is
   type Item_Type is record
      I : Integer;
   end record;

   type Index_Type is (A, B);
   for Index_Type use (A => 1, B => 10);

   Item_Array : constant array (Index_Type) of Item_Type
     := (A => (I => 10), B => (I => 22));

   Item : Item_Type;
   for Item'Address use Item_Array(Index_Type)'Address;
begin
   null;
end;
