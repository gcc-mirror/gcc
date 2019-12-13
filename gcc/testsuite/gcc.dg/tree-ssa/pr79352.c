/* PR tree-optimization/79352 - -fprintf-return-value doesn't handle
   flexible-like array members properly
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized -fno-ipa-icf" } */

struct A { int i; char a1[1]; };
struct B { int i; char a3[3]; };
struct C { int i; char ax[]; };

int test_array_1 (int i, struct A *a)
{
  return __builtin_snprintf (0, 0, "%-s", a->a1);
}

int test_array_3 (int i, struct B *b)
{
  return __builtin_snprintf (0, 0, "%-s", b->a3);
}

int test_array_1_3 (int i, struct A *a, struct B *b)
{
  return __builtin_snprintf (0, 0, "%-s", i ? a->a1 : b->a3);
}

int test_string_and_array_3 (int i, struct B *b)
{
  return __builtin_snprintf (0, 0, "%-s", i ? "123" : b->a3);
}

int test_flexarray (struct C *c)
{
  return __builtin_snprintf (0, 0, "%-s", c->ax);
}

int test_array_and_flexarray (int i, struct B *b, struct C *c)
{
  return __builtin_snprintf (0, 0, "%-s", i ? b->a3 : c->ax);
}

int test_string_and_flexarray (int i, struct C *c)
{
  return __builtin_snprintf (0, 0, "%-s", i ? "123" : c->ax);
}

/* { dg-final { scan-tree-dump-times "snprintf" 7 "optimized"} } */
