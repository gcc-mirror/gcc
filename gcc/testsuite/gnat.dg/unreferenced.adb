--  { dg-do compile }
--  { dg-options "-gnatd.F" }

procedure Unreferenced is
   X : aliased Integer;
   Y : access  Integer := X'Access;
   Z : Integer renames Y.all;
   pragma Unreferenced (Z);
begin
   null;
end Unreferenced;
