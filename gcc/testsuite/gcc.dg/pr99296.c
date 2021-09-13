// { dg-do compile }
// { dg-options "-O2 -fno-tree-bit-ccp" }

struct {
  signed a : 1;
} b, c;
void d() { b.a |= c.a |= 0 != 2; }
