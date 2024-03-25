/* { dg-do compile } */
/* { dg-options "-Wpedantic" } */

/* __builtin_speculation_safe_value returns a value with the same type
   as its first argument.  There should be an error if that isn't
   type-compatible with the use.  */
int *
f (int x)
{
  return __builtin_speculation_safe_value (x);  /* { dg-error "returning 'int' from a function with return type 'int \\*' makes pointer from integer without a cast" } */
}

/* { dg-prune-output "this target does not define a speculation barrier;" } */
