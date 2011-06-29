-- { dg-do compile }
-- { dg-options "-g" }

with Taft_Type3_Pkg; use Taft_Type3_Pkg;

procedure Taft_Type3 is

  subtype S is String (1..32);

  Empty : constant S := (others => ' ');

  procedure Proc (Data : in out T) is begin null; end;

  task type Task_T is
    entry Send (Data : in out T);
  end;

  task body Task_T is
    type List_T is array (1 .. 4) of S;
    L : List_T := (others => Empty);
  begin
    accept Send (Data : in out T) do
      Proc (Data);
    end;
  end;

begin
  null;
end;
