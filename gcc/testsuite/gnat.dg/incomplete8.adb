-- PR ada/114708
-- Reported by Jere <jhb.chat@gmail.com>

-- { dg-do compile }

procedure Incomplete8 is

  generic
    type Element_Type(<>);
  package Test_Incomplete_Formal is
    type Element_Access is access Element_Type;
  end Test_Incomplete_Formal;

  type Node;

  package P is new Test_Incomplete_Formal(Node);

  type Node is limited null record;
   
begin
  null;
end;
