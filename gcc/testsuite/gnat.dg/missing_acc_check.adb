--  { dg-do run }

procedure Missing_Acc_Check is
   
   Test_Failed : Exception;
   
   type Int_Access is access all Integer;
   
   Save : Int_Access := null;
   
   type Int_Rec is record
      Int : aliased Integer;
   end record;
   
   type Ltd_Rec (IR_Acc : access Int_Rec) is limited null record;
   
   function Pass_Rec (IR_Acc : access Int_Rec) return Int_Access is
   begin
      return IR_Acc.Int'Access;  -- Accessibility check here
   end Pass_Rec;
   
   procedure Proc is
      IR : aliased Int_Rec;
      LR : Ltd_Rec (IR'Access);
   begin
      Save := Pass_Rec (LR.IR_Acc);  -- Must raise Program_Error;

      if Save /= null then
         raise Test_Failed;
      end if;
   
   exception
      when Program_Error =>
         null;
   end Proc;

begin
   Proc;
end Missing_Acc_Check;
