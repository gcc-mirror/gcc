-- PR middle-end/36575
-- reporter: Laurent Guerby <laurent@guerby.net>
-- { dg-do run }

procedure Conv_Decimal is

  type Unsigned_Over_8 is mod 2**8+2;
  type Signed_Over_8 is range -200 .. 200;

  procedure Assert(Truth: Boolean) is
  begin
    if not Truth then
      raise Program_Error;
    end if;
  end;

  type Decim is delta 0.1 digits 5;

  Halfway  : Decim := 2.5;
  Neg_Half : Decim := -2.5;

  Big      : Unsigned_Over_8;
  Also_Big : Signed_Over_8;

begin
  Big := Unsigned_Over_8 (Halfway); -- Rounds up by 4.6(33).
  Assert(Big = 3);

  Also_Big := Signed_Over_8 (Halfway); -- Rounds up by 4.6(33).
  Assert(Also_Big = 3);

  Also_Big := Signed_Over_8 (Neg_Half); -- Rounds down by 4.6(33).
  Assert(Also_Big = -3);
end;
