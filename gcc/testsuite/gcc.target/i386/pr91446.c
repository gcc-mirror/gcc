/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake -ftree-slp-vectorize -mtune-ctrl=^sse_typeless_stores" } */

typedef struct
{
  unsigned long long width, height;
  long long x, y;
} info;

extern void bar (info *);

void
foo (unsigned long long width, unsigned long long height,
     long long x, long long y)
{
  info t;
  t.width = width;
  t.height = height;
  t.x = x;
  t.y = y;
  bar (&t);
}

/* { dg-final { scan-assembler-times "vmovdqa\[^\n\r\]*xmm\[0-9\]" 2 } } */
