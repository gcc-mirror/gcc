-- { dg-do compile }
-- { dg-options "-fdump-tree-original" }

package body Aggr12 is

  procedure Print (Data : String) is
  begin
    null;
  end;

  procedure Test is
  begin
    Print (Hair_Color_Type'Image (A.I1));
    Print (Hair_Color_Type'Image (A.I2));
  end;

end Aggr12;

-- { dg-final { scan-tree-dump-not "{.i1=0, .i2=2}" "original" } }
-- { dg-final { cleanup-tree-dump "original" } }
