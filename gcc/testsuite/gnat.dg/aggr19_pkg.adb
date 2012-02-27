package body Aggr19_Pkg is

  procedure Proc (Pool : in out Rec5) is
  begin
    Pool.Ent := (Kind => Two, Node => Pool.Ent.Node, I => 0);
  end;

end ;
