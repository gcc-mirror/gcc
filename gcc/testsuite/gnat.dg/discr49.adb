--  { dg-do run }

with Discr49_Rec2; use Discr49_Rec2;

procedure Discr49 is
   Obj : Child (True);
   I : Integer := Value (Obj) + Boolean'Pos (Obj.Discr);
begin
   if I /= 125 then
      raise Program_Error;
   end if;
end Discr49;
