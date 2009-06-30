-- { dg-do compile }
-- { dg-options "-gnatws" }

with Discr15_Pkg; use Discr15_Pkg;

procedure Discr15 (History : in Rec_Multi_Moment_History) is

  Sub: constant Rec_Multi_Moment_History := Sub_History_Of (History);
  subtype Vec is String(0..Sub.Last);
  Mmts : array(1..Sub.Size) of Vec;

begin
  null;
end;
