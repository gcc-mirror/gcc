/* Checks for a bug where a function with a section attribute would prevent
   all later functions from being partitioned into hot and cold blocks.  */
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

#ifdef __APPLE__
__attribute__ ((section ("__TEXT,__text")))
#else
__attribute__((section(".text")))
#endif
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

/* { dg-final-use { scan-assembler "\.section\[\t \]*\.text\.unlikely\[\\n\\r\]+\[\t \]*\.size\[\t \]*foo\.cold" { target *-*-linux* *-*-gnu* } } } */
/* { dg-final-use { scan-assembler {.section[\t ]*__TEXT,__text_cold[^\n]*[\n\r]+_foo.cold:} { target *-*-darwin* } } } */
