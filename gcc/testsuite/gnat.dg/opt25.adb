-- { dg-do compile }
-- { dg-options "-O" }

with Opt25_Pkg1;
with Opt25_Pkg2;

procedure Opt25 (B1, B2 : in out Boolean) is

   package Local_Pack_Instance is new Opt25_Pkg1 (Boolean, True);

   package Local_Stack is new Opt25_Pkg2 (Integer, 0);

   S : Local_Stack.Stack := Local_Stack.Default_Stack;

begin
   Local_Pack_Instance.Swap (B1, B2);
end;
