/* { dg-do compile } */
/* { dg-options "-O0 -mno-popcnt" } */

inline int __attribute__ ((__gnu_inline__, __always_inline__, target("popcnt")))
foo () /* { dg-error "inlining failed in call to 'always_inline' .* target specific option mismatch" } */
{
  return 0;
}

int bar()
{
  return foo (); /* { dg-message "called from here" } */
}
