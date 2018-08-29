/* PR sanitizer/81186 */
/* { dg-do run } */

int
main ()
{
  __label__ l;
  void f ()
  {
    int a[123];

    goto l;
  }

  f ();
l:
  return 0;
}
