/* { dg-do compile } */
/* { dg-additional-options "-mharden-sls=blr -mbranch-protection=bti" } */
/*
   Ensure that the SLS hardening of BLR leaves no BLR instructions.
   Here we also check that there are no BR instructions with anything except an
   x16 or x17 register.  This is because a `BTI c` instruction can be branched
   to using a BLR instruction using any register, but can only be branched to
   with a BR using an x16 or x17 register.
  */
typedef int (foo) (int, int);
typedef void (bar) (int, int);
struct sls_testclass {
    foo *x;
    bar *y;
    int left;
    int right;
};

/* We test both RTL patterns for a call which returns a value and a call which
   does not.  */
int blr_call_value (struct sls_testclass x)
{
  int retval = x.x(x.left, x.right);
  if (retval % 10)
    return 100;
  return 9;
}

int blr_call (struct sls_testclass x)
{
  x.y(x.left, x.right);
  if (x.left % 10)
    return 100;
  return 9;
}

/* { dg-final { scan-assembler-not {\tblr\t} } } */
/* { dg-final { scan-assembler-not {\tbr\tx(?!16|17)} } } */
/* { dg-final { scan-assembler {\tbr\tx(16|17)} } } */

