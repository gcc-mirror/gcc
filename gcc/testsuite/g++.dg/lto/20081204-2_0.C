// { dg-lto-do link }
// { dg-lto-options {{-flto -flto-partition=1to1 -O3}} }
extern void foo (void);

int
main ()
{
  foo ();
  return 0;
}
