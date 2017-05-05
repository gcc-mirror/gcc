/* { dg-do compile } */
/* { dg-require-effective-target tls } */
/* { dg-require-effective-target arm_cortex_m } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-mslow-flash-data" } */

__thread int x = 0;

int
bar ()
{
  return x;
}

/* { dg-error "accessing thread-local storage is not currently supported with -mpure-code or -mslow-flash-data" "" { target *-*-* } 12 } */
