/* { dg-additional-options "-mharden-sls=blr -save-temps" } */
/* Ensure that the SLS hardening of BLR leaves no BLR instructions.
   We only test that all BLR instructions have been removed, not that the
   resulting code makes sense.  */
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
/* { dg-final { scan-assembler {\tbr\tx[0-9][0-9]?} } } */
