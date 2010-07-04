-- { dg-do run }

with Wide_Boolean_Pkg; use Wide_Boolean_Pkg;

procedure Wide_Boolean is

   R : TREC;
   LB_TEST_BOOL : TBOOL;

begin

   R.B := FALSE;
   LB_TEST_BOOL := FALSE;

   Modify (R.H, R.B);
   if (R.B /= TRUE) then
     raise Program_Error;
   end if;

   Modify (R.H, LB_TEST_BOOL);
   R.B := LB_TEST_BOOL;
   if (R.B /= TRUE) then
     raise Program_Error;
   end if;

end;
