--  { dg-do compile }

with Ada.Finalization;

procedure Protected_Type1 is

  type T is new Ada.Finalization.Controlled with null record;

  protected type Queue is
    entry Get_Next_Line (Line : out String);
  private
    A : T;
  end Queue;

  protected body Queue is
    entry Get_Next_Line (Line : out String)
      when A /= (Ada.Finalization.Controlled with null record) is
    begin
      null;
    end Get_Next_Line;
  end Queue;

begin
  null;
end;
