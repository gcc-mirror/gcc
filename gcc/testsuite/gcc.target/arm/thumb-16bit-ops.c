/* Check that the compiler properly uses 16-bit encodings where available.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-Os -fno-builtin -mthumb" } */

int
f (int a, int b )
{
  return a + b;
}

/* { dg-final { scan-assembler "adds	r0, r0, r1" } } */

int
g1 (int a)
{
  return a + 255;
}

/* { dg-final { scan-assembler "adds	r0, r0, #255" } } */

int
g2 (int a)
{
  return a + 256;
}

/* { dg-final { scan-assembler "add	r0, r0, #256" } } */

int
g3 (int a)
{
  return a - 255;
}

/* { dg-final { scan-assembler "subs	r0, r0, #255" } } */

int
g4 (int a)
{
  return a - 256;
}

/* { dg-final { scan-assembler "sub	r0, r0, #256" } } */

int
h1 (int a, int b)
{
  return b + 7;
}

/* { dg-final { scan-assembler "adds	r0, r1, #7" } } */

int
h2 (int a, int b)
{
  return b + 8;
}

/* { dg-final { scan-assembler "add	r0, r1, #8" } } */

int
h3 (int a, int b)
{
  return b - 7;
}

/* { dg-final { scan-assembler "subs	r0, r1, #7" } } */

int
h4 (int a, int b)
{
  return b - 8;
}

/* { dg-final { scan-assembler "sub	r0, r1, #8" } } */

int
i (int a, int b)
{
  return b;
}

/* { dg-final { scan-assembler "mov	r0, r1" } } */

int
j1 ()
{
  return 255;
}

/* { dg-final { scan-assembler "movs	r0, #255" } } */

int
j2 ()
{
  return 256;
}

/* { dg-final { scan-assembler "mov	r0, #256" } } */

int
k (int a, int b)
{
  return b << 15;
}

/* { dg-final { scan-assembler "lsls	r0, r1, #15" } } */

int
l1 (int a, int b)
{
  return a << b;
}

/* { dg-final { scan-assembler "lsls	r0, r0, r1" } } */

int
l2 (int a, int b, int c)
{
  return b << c;
}

/* { dg-final { scan-assembler "lsl	r0, r1, r2" } } */

int
m (int a, int b)
{
  return b >> 15;
}

/* { dg-final { scan-assembler "asrs	r0, r1, #15" } } */

int
n1 (int a, int b)
{
  return a >> b;
}

/* { dg-final { scan-assembler "asrs	r0, r0, r1" } } */

int
n2 (int a, int b, int c)
{
  return b >> c;
}

/* { dg-final { scan-assembler "asr	r0, r1, r2" } } */

unsigned int
o (unsigned int a, unsigned int b)
{
  return b >> 15;
}

/* { dg-final { scan-assembler "lsrs	r0, r1, #15" } } */

unsigned int
p1 (unsigned int a, unsigned int b)
{
  return a >> b;
}

/* { dg-final { scan-assembler "lsrs	r0, r0, r1" } } */

unsigned int
p2 (unsigned int a, unsigned int b, unsigned int c)
{
  return b >> c;
}

/* { dg-final { scan-assembler "lsr	r0, r1, r2" } } */

int
q (int a, int b)
{
  return b * a;
}

/* { dg-final { scan-assembler "muls	r0, r1, r0" } } */

int
r (int a, int b)
{
  return ~b;
}

/* { dg-final { scan-assembler "mvns	r0, r1" } } */

int
s (int a, int b)
{
  return -b;
}

/* { dg-final { scan-assembler "negs	r0, r1" } } */
