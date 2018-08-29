--  { dg-do compile }

with Suppress_Initialization_Pkg;

procedure Suppress_Initialization is
begin
   Suppress_Initialization_Pkg.Read;
end Suppress_Initialization;
