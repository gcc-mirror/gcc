-- { dg-do compile }

with Aggr32_Pkg.Child;

procedure Aggr32 (W, H : Positive) is

  use Aggr32_Pkg;

  package Test_1 is new Child (Frame => (Width => W, Height => H));

  package Test_2 is new Child (Frame => Rec'(Width => W, Height => H));

begin
  null;
end;
