--  { dg-do compile }

with Deques;
procedure Show_Deques_Priority is
    use Deques;

    PD : aliased P_Deque := Create;

begin
    PD.Pop;
end Show_Deques_Priority;
