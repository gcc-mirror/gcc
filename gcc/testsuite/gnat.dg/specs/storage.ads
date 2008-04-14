-- { dg-do compile }
with System.Pool_Global;
package Storage is
   x1: System.Pool_Global.Unbounded_No_Reclaim_Pool;
   type T1 is access integer;
   for T1'Storage_Pool use (x1);  -- { dg-error "must be a variable" }
   type T2 is access Integer;
   for T2'Storage_Pool use x1;
end Storage;

