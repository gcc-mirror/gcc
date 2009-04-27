-- { dg-do run }
-- { dg-options "-O2 -fno-inline" }

procedure Opt2 is
   function Get return String is
   begin
      return "[]";
   end Get;

   Message : String := Get;

   F, L : Integer;
begin
   for J in Message'Range loop
      if Message (J) = '[' then
         F := J;
      elsif Message (J) = ']' then
         L := J;
         exit;
      end if;
   end loop;

   declare
      M : String :=
         Message (Message'First .. F) & Message (L .. Message'Last);
   begin
      if M /= "[]" then
        raise Program_Error;
      end if;
   end;
end;
