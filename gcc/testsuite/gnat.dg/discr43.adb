-- { dg-do compile }

with Text_IO; use Text_IO;

procedure Discr43 is

  type Arr is array (Short_Integer range <>) of Boolean;

  type Rec (LB : Short_Integer; UB : Short_Integer) is record
    A : Arr (LB .. UB);
  end record;

begin
  Put_Line ("Arr'Max_Size =" & Arr'Max_Size_In_Storage_Elements'Img);
  Put_Line ("Rec'Max_Size =" & Rec'Max_Size_In_Storage_Elements'Img);
end;
