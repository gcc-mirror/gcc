/* PR preprocessor/56824 */
/* { dg-do compile } */
/* { dg-options "-Waggregate-return" } */

struct S { int i; };
struct S foo (void);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Waggregate-return"

int
main ()
{
  foo ();
  return 0;
}

#pragma GCC diagnostic pop
