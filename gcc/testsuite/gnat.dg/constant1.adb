--  { dg-do compile }

procedure Constant1 is
   Def_Const : constant Integer;
   pragma Import (Ada, Def_Const);
begin
   null;
end;
