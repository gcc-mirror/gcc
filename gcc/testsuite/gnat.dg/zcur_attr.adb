--  { dg-do compile }
--  { dg-options "-fdump-tree-optimized" }

package body ZCUR_Attr is
   function F return Integer is (0);
end ZCUR_Attr;

--  { dg-final { scan-tree-dump "zero_call_used_regs \[(\]\"all\"\[)\]" "optimized" } }
