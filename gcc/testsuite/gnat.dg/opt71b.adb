-- { dg-do compile }
-- { dg-require-effective-target store_merge }
-- { dg-options "-O2 -fdump-tree-store-merging" }

with Opt71_Pkg; use Opt71_Pkg;

procedure Opt71b (X : not null access Rec; Y : not null access Rec) is
begin
   X.all := (Flag => True, Size => Y.Size);
end;

-- { dg-final { scan-tree-dump "Merging successful" "store-merging" } }
