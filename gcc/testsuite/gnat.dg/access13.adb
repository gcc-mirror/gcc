-- { dg-do compile }

package body Access13 is

  procedure Set_R (A : access procedure (B : Boolean)) is
  begin
    Global_R := (A => A); -- { dg-error "illegal|deeper|use named" }
  end;

  procedure Attach_Local_To_Global is
    Cond : Boolean;

    procedure Proc (B : Boolean) is
    begin
      Cond := B;
    end;

  begin
    Set_R (Proc'Access);
  end;

end Access13;
