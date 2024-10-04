-- { dg-do compile }

with System;

package body Subp_Elim_Errors is

   type Acc_Proc is access procedure;

   procedure Proc is
   begin
      null;
   end Proc;

   procedure Pass_Proc (P : Acc_Proc) is
   begin
      P.all;
   end Pass_Proc;

   procedure Pass_Proc (P : System.Address) is
   begin
      null;
   end Pass_Proc;

begin
   Proc;                           -- { dg-error "eliminated" }

   Pass_Proc (Proc'Access);        -- { dg-error "eliminated" }

   Pass_Proc (Proc'Address);       -- { dg-error "eliminated" }

   Pass_Proc (Proc'Code_Address);  -- { dg-error "eliminated" }
end Subp_Elim_Errors;
