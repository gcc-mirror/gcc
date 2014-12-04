/* PR middle-end/38338 */
/* { dg-options "-O0" } */
/* { dg-options "-O0 -fPIC" { target fpic } } */
/* { dg-require-effective-target untyped_assembly } */

typedef void (*fnp) (void);

static char
foo (char x)
{
  return x;
}

static void *
bar (char x)
{
  void *args = __builtin_apply_args ();
  return __builtin_apply ((fnp) foo, args, sizeof (void *));
}
