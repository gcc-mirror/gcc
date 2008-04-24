/* Test for printf formats. Test for ISO C warnings with MS "I64"
   extension.*/

/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat -Wno-long-long" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (long long ll, unsigned long long ull, long long *lln,
     long long *llp, unsigned long long *ullp)
{
  printf ("%I64d", ll); /* { dg-warning "'I64' ms_printf length modifier" "printf %I64d" } */
  printf ("%I64i", ll); /* { dg-warning "'I64' ms_printf length modifier" "printf %I64i" } */
  printf ("%I64o", ull); /* { dg-warning "'I64' ms_printf length modifier" "printf %I64o" } */
  printf ("%I64u", ull); /* { dg-warning "'I64' ms_printf length modifier" "printf %I64u" } */
  printf ("%I64x", ull); /* { dg-warning "'I64' ms_printf length modifier" "printf %I64x" } */
  printf ("%I64X", ull); /* { dg-warning "'I64' ms_printf length modifier" "printf %I64X" } */
  printf ("%I64n", lln); /* { dg-warning "'I64' ms_printf length modifier" "printf %I64n" } */
  scanf ("%I64d", llp); /* { dg-warning "'I64' ms_scanf length modifier" "scanf %I64d" } */
  scanf ("%I64i", llp); /* { dg-warning "'I64' ms_scanf length modifier" "scanf %I64i" } */
  scanf ("%I64o", ullp); /* { dg-warning "'I64' ms_scanf length modifier" "scanf %I64o" } */
  scanf ("%I64u", ullp); /* { dg-warning "'I64' ms_scanf length modifier" "scanf %I64u" } */
  scanf ("%I64x", ullp); /* { dg-warning "'I64' ms_scanf length modifier" "scanf %I64x" } */
  scanf ("%I64X", ullp); /* { dg-warning "'I64' ms_scanf length modifier"  "scanf %I64X" } */
  scanf ("%I64n", llp); /* { dg-warning "'I64' ms_scanf length modifier" "scanf %I64n" } */
}
