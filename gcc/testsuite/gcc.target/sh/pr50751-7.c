/* Check that mov.b and mov.w displacement insns are generated.
   If this is working properly, there should be no base address adjustments
   outside the mov insns.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } } */
/* { dg-final { scan-assembler-not "add|sub" } } */

typedef struct 
{
  char	a;
  char	b;
  char	c;
  char	d;

  short e;
  short f;

  int g;
  int h;
} X;

void
testfunc_00 (X* x)
{
  x->g = x->b | x->c;
  x->h = x->e | x->f;
  x->d = x->g;
  x->f = x->h;
}

int testfunc_01 (X* x)
{
  return x->b | x->e | x->g;
}
