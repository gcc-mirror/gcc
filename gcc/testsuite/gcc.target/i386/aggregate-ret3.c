/* Check that, with keep_aggregate_return_pointer attribute,  callee does
   not pop the stack for the implicit pointer arg when returning a large
   structure in memory.  */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */

struct foo {
  int a;
  int b;
  int c;
  int d;
};

__attribute__ ((ms_abi))
struct foo
bar (void)
{
  struct foo retval;
  retval.a = 1;
  retval.b = 2;
  retval.c = 3;
  retval.d = 4;
  return retval;
}

/* { dg-final { scan-assembler-not "ret\[ \t\]\\\$4" } } */


