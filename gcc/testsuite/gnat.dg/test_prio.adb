-- { dg-do run }
-- { dg-options "-gnatws" }

pragma Locking_Policy (Ceiling_Locking);
with test_prio_p;use test_prio_p;
with text_io; use text_io;
procedure Test_Prio is
   task Tsk is
      pragma Priority (10);
   end Tsk;
   task body Tsk is
   begin   
      Sema2.Seize;
      Sema1.Seize;
      Put_Line ("error");
   exception
      when Program_Error => null;  -- OK
   end;    
begin   
   null;   
end;    
