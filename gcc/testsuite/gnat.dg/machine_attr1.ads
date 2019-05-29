package Machine_Attr1 is

  type Arr is array (1 .. 256) of Integer;

  A, B, C : Arr;

  procedure Proc1;
  pragma Machine_Attribute (Proc1, "flatten");

  procedure Proc2;
  pragma Machine_Attribute (Proc2, "used");

  procedure Proc3;
  pragma Machine_Attribute (Proc3, "cold");

  procedure Proc4;
  pragma Machine_Attribute (Proc4, "hot");

  procedure Proc5;
  pragma Machine_Attribute (Proc5, "target", "avx");

  procedure Proc6;
  pragma Machine_Attribute (Proc6, "target_clones", "avx", "avx2", "default");

end Machine_Attr1;
