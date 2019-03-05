/* PR middle-end/88546 - Copy attribute unusable for weakrefs
   Verify that attribute noreturn (represented as volatile on function
   decls) is interpreted correctly and doesn't affect variables.
   { dg-do compile }
   { dg-options "-O1 -Wall -fdump-tree-optimized" }*/

#define ATTR(...)   __attribute__ ((__VA_ARGS__))
#define ASRT(expr)   _Static_assert (expr, #expr)

ATTR (noreturn) void fnoreturn (void);
ATTR (copy (fnoreturn)) void fnoreturn_copy (void);
ASRT (__builtin_has_attribute (fnoreturn_copy, noreturn));

int call_fnoreturn_copy (void)
{
  fnoreturn_copy ();
  fnoreturn_copy ();   // should be eliminated
}

// { dg-final { scan-tree-dump-times "fnoreturn_copy \\(\\);" 1 "optimized" } }


_Noreturn void f_Noreturn (void);
ATTR (copy (f_Noreturn)) void f_Noreturn_copy (void);
ASRT (__builtin_has_attribute (f_Noreturn_copy, noreturn));

int call_f_Noreturn_copy (void)
{
  f_Noreturn_copy ();
  f_Noreturn_copy ();   // should be eliminated
}

// { dg-final { scan-tree-dump-times "f_Noreturn_copy \\(\\);" 1 "optimized" } }


// Verify the combination of both is accepted and works too,
// just for fun.
ATTR (noreturn) _Noreturn void fnoreturn_Noreturn (void);
ATTR (copy (fnoreturn_Noreturn)) void fnoreturn_Noreturn_copy (void);
ASRT (__builtin_has_attribute (fnoreturn_Noreturn_copy, noreturn));

int call_fnoreturn_Noreturn_copy (void)
{
  fnoreturn_Noreturn_copy ();
  fnoreturn_Noreturn_copy ();   // should be eliminated
}

// { dg-final { scan-tree-dump-times "fnoreturn_Noreturn_copy \\(\\);" 1 "optimized" } }


typedef void func_t (void);

ATTR (noreturn) func_t func_noreturn;
ATTR (copy (func_noreturn)) func_t func_noreturn_copy;
ASRT (__builtin_has_attribute (func_noreturn_copy, noreturn));

int call_func_noreturn_copy (void)
{
  func_noreturn_copy ();
  func_noreturn_copy ();   // should be eliminated
}

// { dg-final { scan-tree-dump-times "func_noreturn_copy \\(\\);" 1 "optimized" } }


// Finally, verify that the volatile bit isn't copied for variables.
extern volatile int vi;

int read_nonvolatile (void)
{
  ATTR (copy (vi)) int i = 0;

  return i + i;   // should be folded to return 0;
}

// { dg-final { scan-tree-dump-times "return 0;" 1 "optimized" } }
