--  { dg-do run }

with Ada.Unchecked_Deallocation;
with Unc_Memops;

procedure Unc_Memfree is

   type List is array (Natural range <>) of Integer;
   for List'Alignment use Standard'Maximum_Alignment;

   type Fat_List_Access is access all List;

   type Thin_List_Access is access all List;
   for Thin_List_Access'Size use Standard'Address_Size;

   procedure Release_Fat is new Ada.Unchecked_Deallocation
     (Object => List, Name => Fat_List_Access);

   procedure Release_Thin is new Ada.Unchecked_Deallocation
     (Object => List, Name => Thin_List_Access);

   My_Fat_List : Fat_List_Access;
   My_Thin_List : Thin_List_Access;
begin
   Unc_Memops.Expect_Symetry (True);

   My_Fat_List := new List (1 .. 3);
   Release_Fat (My_Fat_List);

   My_Thin_List := new List (1 .. 3);
   Release_Thin (My_Thin_List);

   Unc_Memops.Expect_Symetry (False);
end;
