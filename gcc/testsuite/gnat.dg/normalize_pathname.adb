--  { dg-do run }

with GNAT.OS_Lib;

procedure Normalize_Pathname is
   S : constant String := GNAT.OS_Lib.Normalize_Pathname
     ("/../tmp", Directory => "", Resolve_Links => True);
begin
   null;
end Normalize_Pathname;
