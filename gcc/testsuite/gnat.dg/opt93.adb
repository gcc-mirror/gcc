-- { dg-do compile { target lp64 } }
-- { dg-options "-O2 -fdump-tree-optimized" }

package body Opt93 is

  function Worker (Obj : T) return Boolean is
  begin
    return (for some J in 1 .. Obj.D2 => Obj.A (J) = 0);
  end;

  function Contains_Zero (Obj : T) return Boolean is
  begin
    return Worker (Obj);
  exception
    when Others => raise Program_Error;
  end;

end Opt93;

-- { dg-final { scan-tree-dump "ivtmp.\[0-9_]+ = ivtmp.\[0-9_]+ \\+ 2" "optimized" } }
