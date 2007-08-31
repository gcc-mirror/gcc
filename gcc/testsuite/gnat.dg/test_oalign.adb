-- { dg-do run }

with System.Storage_Elements; use System.Storage_Elements;
with Oalign1, Oalign2; use Oalign1, Oalign2;

procedure Test_Oalign is
begin
   if Klunk1'Address mod Klunk1'Alignment /= 0 then
      raise Program_Error;
   end if;
   if Klunk2'Address mod Klunk2'Alignment /= 0 then
      raise Program_Error;
   end if;
end;
