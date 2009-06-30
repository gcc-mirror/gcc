-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Discr17 is

  F1_Poe : Integer := 18;

  function F1 return Integer is
  begin
    F1_Poe := F1_Poe - 1;
    return F1_Poe;
 end F1;

  generic
    type T is limited private;
    with function Is_Ok (X : T) return Boolean;
  procedure Check;

  procedure Check is
  begin

    declare
      type Poe is new T;
      X : Poe;
      Y : Poe;
    begin
      null;
    end;

    declare
      type Poe is new T;
      type Arr is array (1 .. 2) of Poe;
      X : Arr;
      B : Boolean := Is_Ok (T (X (1)));
    begin
      null;
    end;

 end;

  protected type Poe (D3 : Integer := F1) is
    entry E (D3 .. F1);    -- F1 evaluated
    function Is_Ok return Boolean;
  end Poe;

  protected body Poe is
    entry E (for I in D3 .. F1) when True is
    begin
      null;
    end E;
    function Is_Ok return Boolean is
    begin
      return False;
    end Is_Ok;
  end Poe;

  function Is_Ok (C : Poe) return Boolean is
  begin
    return C.Is_Ok;
  end Is_Ok;

  procedure Chk is new Check (Poe, Is_Ok);

begin
   Chk;
end;
