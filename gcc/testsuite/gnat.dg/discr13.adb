-- { dg-do compile }

with Discr12_Pkg; use Discr12_Pkg;

procedure Discr13 is

  function F1 return Integer is
  begin
    return Dummy (1);
  end F1;

  protected type Poe (D3 : Integer := F1) is
    entry E (D3 .. F1);    -- F1 evaluated
    function Is_Ok (D3 : Integer; E_First : Integer; E_Last : Integer) return Boolean;
  end Poe;

  protected body Poe is
    entry E (for I in D3 .. F1) when True is
    begin
      null;
    end E;
    function Is_Ok (D3 : Integer; E_First : Integer; E_Last : Integer) return Boolean is
    begin
      return False;
    end Is_Ok;
  end Poe;

begin
  null;
end;
