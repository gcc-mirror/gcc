-- { dg-do compile }
-- { dg-options "-gnatE" }

package body Dynamic_Elab2 is

  function Get_Plot return Plot is

    procedure Fill (X : out Plot) is
    begin
      X.Data := Get_R;
    end;

  X : Plot;

  begin
    Fill(X);
    return X;
  end;

end Dynamic_Elab2;
