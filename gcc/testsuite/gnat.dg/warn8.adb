-- { dg-do compile }

pragma Optimize (Space); -- { dg-warning "must specify -Os" }

procedure Warn8 is
begin
  null;
end;
