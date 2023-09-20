/* { dg-do compile } */
/* { dg-options "" } */

void
foo (int n)
{
  _Static_assert (__builtin_classify_type (enum E { E1, E2 }) == 3, "");
  _Static_assert (__builtin_classify_type (struct S { int s; }) == 12, "");
  _Static_assert (__builtin_classify_type (union U { int u; }) == 13, "");
  _Static_assert (__builtin_classify_type (int [2 * n + 36]) == 14, "");
}
