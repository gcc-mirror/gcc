-- { dg-do compile }
-- { dg-require-stack-check "" }
-- { dg-options "-O -fstack-check" }

package body Stack_Check3 is

  type Int_Arr is array (1 .. 34) of Integer;

  type Rec (D : Boolean := False) is
    record
      case D is
        when True  => IA : Int_Arr;
        when False => null;
      end case;
    end record;

  type Rec_Arr is array (1 .. 256) of Rec;

  protected Prot_Arr is
    procedure Reset;
  private
    A : Rec_Arr;
  end Prot_Arr;

  protected body Prot_Arr is
    procedure Reset is
    begin
      A := (others => (D => False));
    end Reset;
  end Prot_Arr;

  procedure Reset is
  begin
    Prot_Arr.Reset;
  end Reset;

end Stack_Check3;
