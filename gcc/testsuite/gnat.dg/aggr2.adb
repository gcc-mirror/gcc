-- { dg-do compile }

procedure aggr2 is
   task type T_Task;
-- 
   task body T_Task is begin null; end;
-- 
   type Lim_Rec is record
      T : T_Task;
   end record;
-- 
   generic
      Formal : Lim_Rec;
   package P_G is
   end P_G;
-- 
   package P is new P_G (Formal => (T => <>));
begin
   null;
end;

