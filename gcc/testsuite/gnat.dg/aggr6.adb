--  { dg-do compile }

with aggr5;
procedure aggr6 is
   procedure Block is
      Wrapper : aliased aggr5.Q_Action_Event'Class
        := aggr5.Q_Action_Event'Class (aggr5.Build (0));
   begin
      null;
   end; 
begin
   null;
end;    
