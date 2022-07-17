-- { dg-do compile }

package Pack13 is

  generic
    type Value_Type is private;
    Value : in out Value_Type;
  package G is end G;

  type Rec is record
    B : Boolean;
  end record;
  for Rec use record
    B at 0 range 8 .. 8;
  end record;
  for Rec'size use 9;

  type Arr is array (Boolean) of Rec;
  pragma Pack (Arr);

  A : Arr;

  package My_G is new G (Boolean, A(True).B); -- { dg-warning "\"A\" may be referenced before it has a value" }

end Pack13;
