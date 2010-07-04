-- { dg-do compile }
-- { dg-options "-fdump-tree-gimple" }

procedure Aggr13 is

   type A is array (Integer range 1 .. 3) of Short_Short_Integer;

   X : A := (1, 2, 3);

   function F return A is
   begin
      if X /= (1, 2, 3) then
        raise Program_Error;
      end if;
      return (1, 1, 1);
   end;

begin
  X := F;
end;

-- { dg-final { scan-tree-dump-not "= {}" "gimple" } }
-- { dg-final { cleanup-tree-dump "gimple" } }
