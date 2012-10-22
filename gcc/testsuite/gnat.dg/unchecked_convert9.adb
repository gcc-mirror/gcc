-- { dg-do compile }
-- { dg-options "-O -fdump-rtl-final" }

package body Unchecked_Convert9 is

   procedure Proc is
     L : Unsigned_32 := 16#55557777#;
   begin
     Var := Conv (L);
   end;

end Unchecked_Convert9;

-- { dg-final { scan-rtl-dump-times "set \\(mem/v" 1 "final" } }
-- { dg-final { cleanup-rtl-dump "final" } }
