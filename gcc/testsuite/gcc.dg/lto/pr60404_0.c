/* { dg-lto-do run } */
/* { dg-lto-options { { -O1 -flto } } } */
/* { dg-extra-ld-options { -O0 } } */

extern void fn2 (int);
int a[1], b;

int
main ()
{
  fn2 (0);
  if (b != 0 || a[b] != 0)
    __builtin_abort ();
  return 0;
}
