/* { dg-do compile { target lp64 } } */
/* { dg-options "-O1 -mstackrealign -mabi=ms" } */

struct foo
{
  int a;
  int b;
  int c;
  int d;
};

__attribute__ ((sysv_abi))
struct foo bar (void)
{
  struct foo retval;

  retval.a = 1;
  retval.b = 2;
  retval.c = 3;
  retval.d = 4;

  return retval;
}
