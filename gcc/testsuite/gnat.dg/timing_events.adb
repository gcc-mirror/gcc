-- { dg-do run }

procedure Timing_Events is
   type Timing_Event_Handler is access protected procedure;
   
   protected PO is
      entry     Test;
      procedure Proc;
   private
      Data : Integer := 99;
   end PO;
   
   protected body PO is
      entry Test when True is
         Handler : Timing_Event_Handler := Proc'Access;
      begin
         Handler.all;
      end Test;
      
      procedure Proc is
      begin
         if Data /= 99 then
            raise Program_Error;
         end if;
      end Proc;
   end PO;
begin
   PO.Test;
end;
