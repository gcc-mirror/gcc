/* { dg-lto-do run } */
/* { dg-lto-options {{-O2 -flto -flto-partition=1to1} } } */

void exit (int);
__attribute__ ((noreturn))
int
call_me (void)
{
  exit (0);
}
