// Check that the initialization guard variable is an 8-byte aligned,
// 8-byte doubleword and that only the least significant bit is used
// for initialization guard variables.
// { dg-do compile { target aarch64*-*-* } }
// { dg-options "-O -fdump-tree-original -fno-section-anchors" }

int bar();

int *foo ()
{
  static int x = bar ();
  return &x;
}

// { dg-final { scan-assembler _ZGVZ3foovE1x,8,8 } }
// { dg-final { scan-tree-dump "& 1" "original" } }
