--  { dg-do run }
--  { dg-options "-gnata" }

with Default_Initial_Condition_Pack; use Default_Initial_Condition_Pack;

procedure Default_Initial_Condition is
   Obj : T;
begin
   if not DIC_Called then
      raise Program_Error;
   end if;
end Default_Initial_Condition;
