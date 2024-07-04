-- { dg-do compile }
-- { dg-options "-O -gnatn -fdump-tree-optimized" }

package body Opt103 is

  function Read return Mode is
    S : String := Get;
    M : Mode;

  begin
    --  There should be a single call to Value_Enumeration_Pos after inlining

    if Mode'Valid_Value (S) then
      M := Mode'Value (S);
    else
      raise Program_Error;
    end if;

    return M;
  end;

  function Translate (S : String) return Mode is
    M : Mode;

  begin
    --  There should be a single call to Value_Enumeration_Pos after inlining

    if Mode'Valid_Value (S) then
      M := Mode'Value (S);
    else
      raise Program_Error;
    end if;

    return M;
  end;

end Opt103;

-- { dg-final { scan-tree-dump-times ".value_enumeration_pos" 2 "optimized"  } }
