/* { dg-do compile } */
/* { dg-options "-O2 -fdelete-null-pointer-checks" } */
/* { dg-require-weak "" } */

/* { dg-skip-if "" keeps_null_pointer_checks } */
extern int a; /* { dg-error "declared weak after being used" } */
int
t()
{
  return &a!=0;
}
extern int a __attribute__ ((weak));
