-- { dg-do compile }
-- { dg-options "-gnatN" }

with inline_scope_p;
procedure inline_scope (X : Integer) is
   type A is array (Integer range 1 .. 2) of Boolean;
   S : A;  
   pragma Warnings (Off, S);
   procedure Report_List  is
   begin   
      inline_scope_p.Assert (S (1), Natural'Image (Natural (1)));
   end Report_List;
begin   
   null;   
end;    
