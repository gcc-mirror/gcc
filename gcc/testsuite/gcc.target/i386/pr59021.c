/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mvzeroupper" } */

extern void abort (void);

struct S {
  int i1;
  int i2;
  int i3;
};

typedef double v4df  __attribute__ ((vector_size (32)));

extern int foo (v4df, int i1, int i2, int i3, int i4, int i5, struct S s);

void bar (v4df v, struct S s)
{
  int r = foo (v, 1, 2, 3, 4, 5, s);
  if (r)
    abort ();
}

/* { dg-final { scan-assembler-not "vzeroupper" } } */
