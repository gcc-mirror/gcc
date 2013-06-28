/* { dg-do compile } */
/* { dg-options "-mavx -mvzeroupper -mno-fp-ret-in-387" }

/* { dg-error "x87 register return with x87 disabled" "" { target { ! ia32 } } 8 } */

long double
foo (long double x)
{
  return __builtin_ilogbl (x);
}
