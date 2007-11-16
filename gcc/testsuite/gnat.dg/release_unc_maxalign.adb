-- { dg-do run }

with Ada.Unchecked_Deallocation;

procedure Release_UNC_Maxalign is

   type List is array (Natural range <>) of Integer;
   for List'Alignment use Standard'Maximum_Alignment;

   type List_Access is access all List;

   procedure Release is new Ada.Unchecked_Deallocation
     (Object => List, Name => List_Access);

   My_List : List_Access;
begin
   My_List := new List (1 .. 3);
   Release (My_List);
end;
