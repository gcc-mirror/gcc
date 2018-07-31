-- { dg-do compile }
-- { dg-require-effective-target store_merge }
-- { dg-options "-O2 -fdump-tree-store-merging" }

with Opt72_Pkg; use Opt72_Pkg;

procedure Opt72a (X : not null access Rec; Size : Positive) is
begin
   X.all := (Flag => True, Size => Size);
end;

-- { dg-final { scan-tree-dump "Merging successful" "store-merging" } }
