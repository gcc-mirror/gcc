/* { dg-lto-do run } */
/* { dg-lto-options {{-O2 -fwhopr} } } */

void exit (int);
__attribute__ ((noreturn))
int
call_me (void)
{
  exit (0);
}
