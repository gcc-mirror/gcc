-- { dg-do compile }
-- { dg-options "-O1" }
with DECL_CTX_Def; use DECL_CTX_Def;
package body DECL_CTX_Use is
   procedure Check_1 is
   begin
      raise X;
   end;

   procedure Check_2 is
   begin
      raise X;
   end;
end;
