--  { dg-do run }
--
--  This test checks that we can allocate more than 2GB on systems with word
--  sizes larger than 32-bits

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Sec_Stack1 is
   function Get_A_Big_String return String;
   --  Return a very close to 2GB string on the secondary stack that would
   --  overflow the secondary stack if we still had a 2GB limit.

   function Get_A_Big_String return String is
      String_Size : constant Natural := Natural'Last;
   begin
      return String_Size * 'a';
   end Get_A_Big_String;

begin
   --  This test only works on systems with more than 32-bits
   if Standard'Address_Size > 32 then
      declare
         R : String := Get_A_Big_String;
      begin null; end;
   end if;
end Sec_Stack1;
