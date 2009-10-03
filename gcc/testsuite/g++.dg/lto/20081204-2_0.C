// { dg-lto-do link }
// { dg-lto-options {{-fwhopr -O3}} }
extern void foo (void);

int
main ()
{
  foo ();
  return 0;
}
