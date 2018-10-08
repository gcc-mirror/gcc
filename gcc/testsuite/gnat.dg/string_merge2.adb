-- { dg-do compile }
-- { dg-require-effective-target string_merging }
-- { dg-options "-O1 -fmerge-all-constants" }

procedure String_Merge2 is
   procedure Process (X : String);
   pragma Import (Ada, Process);
begin
   Process ("ABCD" & Ascii.NUL);
end;

-- We expect something like:

-- .section  .rodata.str1.1,"aMS",@progbits,1
-- .LC1:
--     .string "ABCD"

-- { dg-final { scan-assembler-times "\\.rodata\\.str" 1 } }
