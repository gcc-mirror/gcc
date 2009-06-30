-- { dg-do compile }

with Discr12_Pkg; use Discr12_Pkg;

procedure Discr12 is

  subtype Small_Int is Integer range 1..10;

  package P is

    type PT_W_Disc (D : Small_Int) is private;

    type Rec_W_Private (D1 : Integer) is
    record
      C : PT_W_Disc (D1);
    end record;

    type Rec_01 (D3 : Integer) is
    record
      C1 : Rec_W_Private (D3);
    end record;

    type Arr is array (1 .. 5) of Rec_01(Dummy(0));

  private
    type PT_W_Disc (D : Small_Int) is 
    record
      Str : String (1 .. D);
    end record;

  end P;

begin
  Null;
end;
