--  { dg-do run }
--  { dg-options "-gnatwae" }

procedure warn1 is
   pragma Warnings
     (Off, "variable ""Unused"" is never read and never assigned");
   Unused : Integer;
   pragma Warnings
     (On, "variable ""Unused"" is never read and never assigned");
begin
   null;
end warn1;
