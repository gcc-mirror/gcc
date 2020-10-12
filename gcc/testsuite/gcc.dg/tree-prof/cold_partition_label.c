/* Test case to check if function foo gets split and the cold function
   gets a label.  */
/* { dg-require-effective-target freorder } */
/* { dg-options "-O2 -freorder-blocks-and-partition -save-temps -fdump-tree-optimized" } */

#ifdef FOR_AUTOFDO_TESTING
#define MAXITER 1000000
#else
#define MAXITER 10000
#endif

#define SIZE 10000

const char *sarr[SIZE];
const char *buf_hot;
const char *buf_cold;

__attribute__((noinline))
void 
foo (int path)
{
  int i;
  if (path)
    {
      for (i = 0; i < SIZE; i++)
	sarr[i] = buf_hot;
    }
  else
    {
      for (i = 0; i < SIZE; i++)
	sarr[i] = buf_cold;
    }
}

int
main (int argc, char *argv[])
{
  int i;
  buf_hot =  "hello";
  buf_cold = "world";
  for (i = 0; i < MAXITER; i++)
    foo (argc);
  return 0;
}

/* { dg-final-use { scan-assembler "foo\[._\]+cold" { target *-*-linux* *-*-gnu* } } } */
/* { dg-final-use { scan-assembler "size\[ \ta-zA-Z0-0\]+foo\[._\]+cold" { target *-*-linux* *-*-gnu* } } } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
