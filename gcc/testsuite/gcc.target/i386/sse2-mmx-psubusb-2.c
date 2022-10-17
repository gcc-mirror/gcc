/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef char v8qi __attribute__ ((vector_size (8)));

char foo()
{
  v8qi tx = { 5, 0, 0, 0, 0, 0, 0, 0 };
  v8qi ty = { 2, 0, 0, 0, 0, 0, 0, 0 };
  v8qi t = __builtin_ia32_psubusb(tx, ty);
  return t[0];
}

char bar()
{
  v8qi tx = { 100, 0, 0, 0, 0, 0, 0, 0 };
  v8qi ty = { 200, 0, 0, 0, 0, 0, 0, 0 };
  v8qi t = __builtin_ia32_psubusb(tx, ty);
  return t[0];
}

/* { dg-final { scan-assembler-times "movl\[ \\t\]+\\\$3," 1 } } */
/* { dg-final { scan-assembler-times "xorl\[ \\t\]+" 1 } } */
/* { dg-final { scan-assembler-not "psubusb\[ \\t\]+%xmm\[0-9\]+" } } */

