/* { dg-do compile } */
/* { dg-options "-O2" }  */

struct X
{
  int c;
};

extern void bar(struct X *);

void foo ()
{
  struct X x;
  bar (&x);
  bar (&x);
  bar (&x);
}

/* { dg-final { scan-assembler-times "r0,\[\\t \]*sp" 3 } } */
