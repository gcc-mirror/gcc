/* { dg-lto-options {{-flto -flto-partition=1to1 -O3}} } */

extern void exit (int);
extern void *foo (void);

void *p;

int
main ()
{
  p = foo ();
  exit (0);
}
