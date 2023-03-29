/* PR sanitizer/109050 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds -fno-sanitize-recover=all" } */

int i;
int foo (void) { return ++i; }

int
main ()
{
  char a[10] = { };
  a[foo ()] = a[foo()] | 'a';
  if (i != 2)
    __builtin_abort ();
  a[foo()] |= 'a';
  if (i != 3)
    __builtin_abort ();
}
