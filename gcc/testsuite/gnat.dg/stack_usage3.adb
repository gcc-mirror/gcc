-- { dg-do compile }
-- { dg-options "-O -fstack-usage" }

with Ada.Text_IO; use Ada.Text_IO;
with Stack_Usage3_Pkg; use Stack_Usage3_Pkg;

procedure Stack_Usage3 is

begin
   Put_Line (Diag ("Diag line 0"));
   Put_Line (Diag ("Diag line 1"));
   Put_Line (Diag ("Diag line 2"));
   Put_Line (Diag ("Diag line 3"));
   Put_Line (Diag ("Diag line 4"));
   Put_Line (Diag ("Diag line 5"));
   Put_Line (Diag ("Diag line 6"));
   Put_Line (Diag ("Diag line 7"));
   Put_Line (Diag ("Diag line 8"));
   Put_Line (Diag ("Diag line 9"));
   Put_Line (Diag ("Diag line 10"));
   Put_Line (Diag ("Diag line 11"));
   Put_Line (Diag ("Diag line 12"));
   Put_Line (Diag ("Diag line 13"));
   Put_Line (Diag ("Diag line 14"));
end;

-- { dg-final { scan-stack-usage "\t\[0-9\]\[0-9\]\t" { target i?86-*-* x86_64-*-* } } }
-- { dg-final { cleanup-stack-usage } }
