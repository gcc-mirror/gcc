/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cfg" } */

#include <limits.h>

void this_comparison_is_false (void);
void this_comparison_is_true (void);
void this_comparison_is_not_decidable (void);

void bla1eq (int var)
{
  if (var + 10 == INT_MIN + 9)
    this_comparison_is_false ();
}

void bla2eq (int var)
{
  if (var + 10 == INT_MIN + 10)
    this_comparison_is_not_decidable ();
}

void bla3eq (int var)
{
  if (var - 10 == INT_MAX - 9)
    this_comparison_is_false ();
}

void bla4eq (int var)
{
  if (var - 10 == INT_MAX - 10)
    this_comparison_is_not_decidable ();
}

void bla1ne (int var)
{
  if (var + 10 != INT_MIN + 9)
    this_comparison_is_true ();
}

void bla2ne (int var)
{
  if (var + 10 != INT_MIN + 10)
    this_comparison_is_not_decidable ();
}

void bla3ne (int var)
{
  if (var - 10 != INT_MAX - 9)
    this_comparison_is_true ();
}

void bla4ne (int var)
{
  if (var - 10 != INT_MAX - 10)
    this_comparison_is_not_decidable ();
}

void bla1lt (int var)
{
  if (var + 10 < INT_MIN + 10)
    this_comparison_is_false ();
}

void bla2lt (int var)
{
  if (var + 10 < INT_MIN + 11)
    this_comparison_is_not_decidable ();
}

void bla3lt (int var)
{
  if (var - 10 < INT_MAX - 9)
    this_comparison_is_true ();
}

void bla4lt (int var)
{
  if (var - 10 < INT_MAX - 10)
    this_comparison_is_not_decidable ();
}

void bla1le (int var)
{
  if (var + 10 <= INT_MIN + 9)
    this_comparison_is_false ();
}

void bla2le (int var)
{
  if (var + 10 <= INT_MIN + 10)
    this_comparison_is_not_decidable ();
}

void bla3le (int var)
{
  if (var - 10 <= INT_MAX - 10)
    this_comparison_is_true ();
}

void bla4le (int var)
{
  if (var - 10 <= INT_MAX - 11)
    this_comparison_is_not_decidable ();
}

void bla1gt (int var)
{
  if (var + 10 > INT_MIN + 9)
    this_comparison_is_true ();
}

void bla2gt (int var)
{
  if (var + 10 > INT_MIN + 10)
    this_comparison_is_not_decidable ();
}

void bla3gt (int var)
{
  if (var - 10 > INT_MAX - 10)
    this_comparison_is_false ();
}

void bla4gt (int var)
{
  if (var - 10 > INT_MAX - 11)
    this_comparison_is_not_decidable ();
}

void bla1ge (int var)
{
  if (var + 10 >= INT_MIN + 10)
    this_comparison_is_true ();
}

void bla2ge (int var)
{
  if (var + 10 >= INT_MIN + 11)
    this_comparison_is_not_decidable ();
}

void bla3ge (int var)
{
  if (var - 11 >= INT_MAX - 10)
    this_comparison_is_false ();
}

void bla4ge (int var)
{
  if (var - 10 >= INT_MAX - 10)
    this_comparison_is_not_decidable ();
}

/* { dg-final { scan-tree-dump-times "this_comparison_is_false" 0 "cfg" } } */
/* { dg-final { scan-tree-dump-times "this_comparison_is_true" 6 "cfg" } } */
/* { dg-final { scan-tree-dump-times "this_comparison_is_not_decidable" 12 "cfg" } } */
/* { dg-final { scan-tree-dump-times "if " 12 "cfg" } } */

