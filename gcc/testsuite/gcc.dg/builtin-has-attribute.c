/* Verify that defining a type in __builtin_has_attribute triggers
   the expected -Wc++-compat warning and evaluates as expected.
   Also verify that the expression in __builtin_has_attribute is
   not evaluated.

  { dg-do compile }
  { dg-options "-O2 -Wall -Wc++-compat -fdump-tree-optimized -ftrack-macro-expansion=0" }  */

#define ATTR(list) __attribute__ (list)

#define A(expect, sym, attr)						\
  typedef int Assert [1 - 2 * !(__builtin_has_attribute (sym, attr) == expect)]

#define assert(expr)						\
  ((expr)							\
   ? (void)0							\
   : (__builtin_printf ("Assertion failed on line %i: %s\n",	\
			__LINE__, #expr),			\
      ++nfails))

A (0, struct A { int i; }, aligned);   /* { dg-warning "expression is invalid in C\\\+\\\+" } */
A (1, struct ATTR ((aligned)) B { int i; }, aligned);   /* { dg-warning "expression is invalid in C\\\+\\\+" } */


static int f (void)
{
  __builtin_abort ();
}

int main (void)
{
  int n = 0, nfails = 0;

  assert (0 == __builtin_has_attribute (int[n++], aligned));
  assert (1 == __builtin_has_attribute (ATTR ((aligned)) int[n++], aligned));
  assert (1 == __builtin_has_attribute (ATTR ((aligned)) int[f ()], aligned));
  assert (1 == 1);

  if (n)
    __builtin_abort ();

  if (nfails)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "abort" 0 "optimized" } } */
