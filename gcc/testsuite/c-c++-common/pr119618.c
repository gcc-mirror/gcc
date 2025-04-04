/* PR gcov-profile/119618 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-fcompare-debug -fprofile-generate -O1" } */
/* { dg-require-profiling "-fprofile-generate" } */

struct S { char s; };
int foo (void);
int *(*fn) (void);

int *
bar (void)
{
  if (foo ())
    return 0;
  {
    struct S s;
    do
      [[gnu::musttail]] return fn ();
    while (0);
  }
}
