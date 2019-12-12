/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target arm_cortex_m } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-skip-if "-mslow-flash-data and -mword-relocations incompatible" { *-*-* } { "-mword-relocations" } } */
/* { dg-options "-mslow-flash-data" } */

__thread int x = 0;

int
bar ()
{
  return x; /* { dg-message "sorry, unimplemented: accessing thread-local storage is not currently supported with -mpure-code or -mslow-flash-data" } */
}

