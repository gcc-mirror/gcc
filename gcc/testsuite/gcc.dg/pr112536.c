/* PR tree-optimization/112536 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf" } */
/* { dg-additional-options "-mlzcnt -mavx512cd -mavx512vl" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-assembler-times "\tvplzcntd\t" 3 { target { i?86-*-* x86_64-*-* } } } } */

unsigned a[12];

void
foo (void)
{
  int i = a[0];
  int j = a[1];
  int k = a[2];
  int l = a[3];
  int e = i ? __builtin_clz (i) : __SIZEOF_INT__ * __CHAR_BIT__;
  int f = j ? __builtin_clz (j) : __SIZEOF_INT__ * __CHAR_BIT__;
  int g = k ? __builtin_clz (k) : __SIZEOF_INT__ * __CHAR_BIT__;
  int h = l ? __builtin_clz (l) : __SIZEOF_INT__ * __CHAR_BIT__;
  a[0] = e;
  a[1] = f;
  a[2] = g;
  a[3] = h;
}

void
bar (void)
{
  int i = a[4];
  int j = a[5];
  int k = a[6];
  int l = a[7];
  int e = i ? __builtin_clz (i) : __SIZEOF_INT__ * __CHAR_BIT__;
  int f = __builtin_clz (j);
  int g = __builtin_clz (k);
  int h = l ? __builtin_clz (l) : __SIZEOF_INT__ * __CHAR_BIT__;
  a[4] = e;
  a[5] = f;
  a[6] = g;
  a[7] = h;
}

void
baz (void)
{
  int i = a[8];
  int j = a[9];
  int k = a[10];
  int l = a[11];
  int e = __builtin_clz (i);
  int f = j ? __builtin_clz (j) : __SIZEOF_INT__ * __CHAR_BIT__;
  int g = __builtin_clz (k);
  int h = l ? __builtin_clz (l) : __SIZEOF_INT__ * __CHAR_BIT__;
  a[8] = e;
  a[9] = f;
  a[10] = g;
  a[11] = h;
}
