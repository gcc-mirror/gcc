--  { dg-do run }

with Ada.Tags;
procedure tag1 is
   type T is tagged null record;
   X : Ada.Tags.Tag;
begin
   begin
     X := Ada.Tags.Descendant_Tag ("Internal tag at 16#0#", T'Tag);
     raise Program_Error;
   exception
     when Ada.Tags.Tag_Error => null; 
   end;
   begin
     X := Ada.Tags.Descendant_Tag ("Internal tag at 16#XXXX#", T'Tag);
     raise Program_Error;
   exception
     when Ada.Tags.Tag_Error => null;
   end; 
end;    
