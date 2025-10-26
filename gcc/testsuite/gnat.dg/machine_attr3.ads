package Machine_Attr3 is

  procedure Proc;

private

  Attr : constant String := "nothrow";
  pragma Machine_Attribute (Proc, Attr);

end Machine_Attr3;
