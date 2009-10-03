/* { dg-lto-options {{-fwhopr -O3}} } */

extern void exit (int);
extern void *foo (void);

void *p;

int
main ()
{
  p = foo ();
  exit (0);
}
