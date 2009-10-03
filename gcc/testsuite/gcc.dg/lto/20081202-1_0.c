/* { dg-lto-options {{-fwhopr -O3}} } */

extern void exit (int);
extern void foo (void);

int
main ()
{
  foo ();
  exit (0);
}
