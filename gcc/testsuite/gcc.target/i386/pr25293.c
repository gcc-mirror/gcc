/* PR target/25293 */
/* { dg-do compile } */
/* { dg-options "-mpreferred-stack-boundary=2 -mtune=i586 -O2 -fomit-frame-pointer -g" } */
/* { dg-require-effective-target ia32 } */

struct T { unsigned short t1, t2, t3, t4, t5, t6, t7; };
struct S { struct T s1; unsigned short s2, s3; };
unsigned short v1;
int f1 (void);
int f2 (struct T);
int f3 (const char *);

int
foo (struct S *x, struct T y)
{
  unsigned short a, b, c;
  unsigned long d, e;
  int f = 0;
  y.t6 = 6;
  a = y.t7;
  b = y.t6;
  c = y.t7;
  switch (a)
    {
    case 8:
    case 7:
      c = 9;
      break;
    case 1:
    case 6:
    case 3:
      b = 16;
      c = 9;
      break;
    }
  if ((f = f1 ()))
    goto error;
  if ((f = f2 (y)))
    goto error;
  d = (long) &y;
  e = (long) &x->s1;
  __asm __volatile ("" : "+D" (e), "+S" (d) :: "memory");
  x->s2 = b;
  x->s3 = c;
  f3 ("foo");
  return 0;
error:
  if (v1 >= 1)
    f3 ("bar");
  return f;
}
