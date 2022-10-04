/* Checks for a bug where static data with a section attribute within a
   function would stop the function being partitioned into hot and cold
   blocks.  */
/* { dg-require-effective-target freorder } */
/* { dg-options "-O2 -fno-profile-reorder-functions -freorder-blocks-and-partition -save-temps" } */

#ifdef FOR_AUTOFDO_TESTING
#define MAXITER 1000000
#else
#define MAXITER 10000
#endif

#define SIZE 10000

#define NOINLINE __attribute__((noinline)) __attribute__ ((noclone))

const char *sarr[SIZE];
const char *buf_hot;
const char *buf_cold;

void foo (int path);

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

void NOINLINE
foo (int path)
{
#ifdef __APPLE__
  static int i __attribute__ ((section ("__DATA,__data")));
#else
  static int i __attribute__((section(".data")));
#endif
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

/* { dg-final-use-not-autofdo { scan-assembler "\.section\[\t \]*\.text\.unlikely\[\\n\\r\]+\[\t \]*\.size\[\t \]*foo\.cold" { target *-*-linux* *-*-gnu* } } } */
/* { dg-final-use-not-autofdo { scan-assembler {.section[\t ]*__TEXT,__text_cold[^\n]*[\n\r]+_foo.cold:} { target *-*-darwin* } } } */
