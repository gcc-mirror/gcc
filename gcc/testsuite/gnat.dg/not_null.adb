--  { dg-do run }

procedure not_null is
   type Not_Null_Int_Ptr is not null access all Integer;
   
   generic
      F : Not_Null_Int_Ptr := null;
   package GPack is
   end GPack;

begin
   declare
      pragma Warnings (Off, "*null not allowed in null-excluding objects");
      pragma Warnings (Off, """Constraint_Error"" will be raised at run time");
      package Inst_2 is new GPack (null);
      pragma Warnings (On, "*null not allowed in null-excluding objects");
      pragma Warnings (On, """Constraint_Error"" will be raised at run time");
   begin
      null;
   end;
exception
   when Constraint_Error =>
      null;
end not_null;
