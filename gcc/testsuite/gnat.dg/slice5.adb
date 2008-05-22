--  { dg-do compile }
--  { dg-options "-gnatwr" }

procedure Slice5 is
   
   type Item_Type is record
      I : Integer;
   end record;
   
   type Index_Type is (A, B);

   type table is array (integer range <>) of integer;
   subtype Small is Integer range 1 .. 10;
   T1 : constant Table (Small) := (Small => 0);
   T2 : constant Table (Small) := T1 (Small);  -- { dg-warning "redundant slice denotes whole array" }
   
   Item_Array : constant array (Index_Type) of Item_Type
     := (A => (I => 10), B => (I => 22));

   Item : Item_Type;
   for Item'Address use Item_Array(Index_Type)'Address;   -- { dg-warning "redundant slice denotes whole array" }
begin
   null;
end;
