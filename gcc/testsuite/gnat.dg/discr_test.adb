--  { dg-do compile }

procedure Discr_Test is
  procedure P is begin null; end P;

  task type Tsk1 is
    entry rvT;
  end Tsk1;

  task body Tsk1 is
  begin
    accept rvT;
  end Tsk1;

  task type Tsk2 (pS : not null access procedure) is
    entry rvT;
  end Tsk2;

  task body Tsk2 is
    tskT : Tsk1;
  begin
    accept rvT do
      requeue tskT.rvT;
    end rvT;
    pS.all;
  end;

  Obj : Tsk2 (P'access);
begin
  Obj.rvT;
end;
