--  { dg-do run }

with Ada.Text_Io; use Ada.Text_Io;
        
procedure requeue1 is

  protected P is
    entry Requeue_Without_Abort;
    entry Queue_Without;
    procedure Open;
  private
    Opened: Boolean := False;
  end P;
 
  protected body P is
    entry Requeue_Without_Abort when True is
    begin
      -- BUG: after this requeue no time out of the call should be possible
      requeue Queue_Without;
    end Requeue_Without_Abort;

    entry Queue_Without when Opened is
    begin
      Opened := False;
    end Queue_Without;

    procedure Open is
    begin
      Opened := True;
    end Open;
  end P;

  -- Test of timed entry call to an entry with requeue without abort
  task T_Without;
  task body T_Without is
  begin
    select
      P.Requeue_Without_Abort;
    or
      delay 1.0;
      Put_Line("failed");
    end select;

  exception
    when others => Put_Line ("failed");
  end T_Without;

begin
  delay 3.0;
  P.Open;
end;
