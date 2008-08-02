-- { dg-do run }

procedure Boolean_Expr2 is

  function Ident_Bool (B : Boolean) return Boolean is
  begin
    return B;
  end;

begin
  if Boolean'Succ (Ident_Bool(False)) /= True then
    raise Program_Error;
  end if;

  if Boolean'Pred (Ident_Bool(True)) /= False then
    raise Program_Error;
  end if;
end;
