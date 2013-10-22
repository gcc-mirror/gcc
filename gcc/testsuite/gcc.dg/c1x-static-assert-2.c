/* Test C11 static assertions.  Failed assertions.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

_Static_assert (0, "assert1"); /* { dg-error "static assertion failed: \"assert1\"" } */

enum e { E0, E1 };

_Static_assert (E0, L"assert2"); /* { dg-error "static assertion failed: \"assert2\"" } */

_Static_assert (-0, "ass" L"ert3"); /* { dg-error "static assertion failed: \"assert3\"" } */

struct s
{
  int a;
  _Static_assert (0, "assert4"); /* { dg-error "static assertion failed: \"assert4\"" } */
  int b;
};

union u
{
  int i;
  _Static_assert ((int)0.0, L"assert5"); /* { dg-error "static assertion failed: \"assert5\"" } */
};

void
f (void)
{
  int i;
  i = 1;
  _Static_assert (0 + 0, "assert6"); /* { dg-error "static assertion failed: \"assert6\"" } */
  i = 2;
}

void
g (void)
{
  int i = 0;
  for (_Static_assert (0, "assert7"); i < 10; i++) /* { dg-error "static assertion failed: \"assert7\"" } */
    ;
}
