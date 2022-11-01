/* { dg-do compile } */
/* { dg-options "-fgimple" } */

unsigned a;
static double *d;
static _Bool b;
__GIMPLE int
foo (int n)
{
  b = __builtin_add_overflow (n, *d, &a);
} /* { dg-error "invalid argument" } */

/* { dg-message "" "" { target *-*-* } 0 } */
