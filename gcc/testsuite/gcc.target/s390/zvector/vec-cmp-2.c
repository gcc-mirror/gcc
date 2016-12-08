/* Similiar to vec-cmp-1.c but requires that
   s390_canonicalize_comparison is able to merge the the two nested
   compares.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector -fno-asynchronous-unwind-tables" } */

#include <vecintrin.h>

extern void foo (void);

int __attribute__((noinline,noclone))
all_eq_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_all_eq (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_eq_double:\n\tvfcedbs\t%v\[0-9\]*,%v24,%v26\n\tjne 1 } } */

int __attribute__((noinline,noclone))
all_ne_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_all_ne (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_ne_double:\n\tvfcedbs\t%v\[0-9\]*,%v24,%v26\n\tjle 1 } } */

int __attribute__((noinline,noclone))
all_gt_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_all_gt (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_gt_double:\n\tvfchdbs\t%v\[0-9\]*,%v24,%v26\n\tjne 1 } } */

int __attribute__((noinline,noclone))
all_lt_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_all_lt (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_lt_double:\n\tvfchdbs\t%v\[0-9\]*,%v26,%v24\n\tjne 1 } } */

int __attribute__((noinline,noclone))
all_ge_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_all_ge (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_ge_double:\n\tvfchedbs\t%v\[0-9\]*,%v24,%v26\n\tjne 1 } } */

int __attribute__((noinline,noclone))
all_le_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_all_le (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_le_double:\n\tvfchedbs\t%v\[0-9\]*,%v26,%v24\n\tjne 1 } } */

int __attribute__((noinline,noclone))
any_eq_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_any_eq (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_eq_double:\n\tvfcedbs\t%v\[0-9\]*,%v24,%v26\n\tjnle 1 } } */

int __attribute__((noinline,noclone))
any_ne_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_any_ne (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_ne_double:\n\tvfcedbs\t%v\[0-9\]*,%v24,%v26\n\tje 1 } } */

int __attribute__((noinline,noclone))
any_gt_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_any_gt (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_gt_double:\n\tvfchdbs\t%v\[0-9\]*,%v24,%v26\n\tjnle 1 } } */

int __attribute__((noinline,noclone))
any_lt_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_any_lt (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_lt_double:\n\tvfchdbs\t%v\[0-9\]*,%v26,%v24\n\tjnle 1 } } */

int __attribute__((noinline,noclone))
any_ge_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_any_ge (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_ge_double:\n\tvfchedbs\t%v\[0-9\]*,%v24,%v26\n\tjnle 1 } } */

int __attribute__((noinline,noclone))
any_le_double (vector double a, vector double b)
{
  if (__builtin_expect (vec_any_le (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_le_double:\n\tvfchedbs\t%v\[0-9\]*,%v26,%v24\n\tjnle 1 } } */

int __attribute__((noinline,noclone))
all_eq_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_all_eq (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_eq_int:\n\tvceqfs\t%v\[0-9\]*,%v24,%v26\n\tjne 1 } } */

int __attribute__((noinline,noclone))
all_ne_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_all_ne (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_ne_int:\n\tvceqfs\t%v\[0-9\]*,%v24,%v26\n\tjle 1 } } */

int __attribute__((noinline,noclone))
all_gt_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_all_gt (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_gt_int:\n\tvchfs\t%v\[0-9\]*,%v24,%v26\n\tjne 1 } } */

int __attribute__((noinline,noclone))
all_lt_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_all_lt (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_lt_int:\n\tvchfs\t%v\[0-9\]*,%v26,%v24\n\tjne 1 } } */

int __attribute__((noinline,noclone))
all_ge_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_all_ge (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_ge_int:\n\tvchfs\t%v\[0-9\]*,%v26,%v24\n\tjle 1 } } */

int __attribute__((noinline,noclone))
all_le_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_all_le (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times all_le_int:\n\tvchfs\t%v\[0-9\]*,%v24,%v26\n\tjle 1 } } */

int __attribute__((noinline,noclone))
any_eq_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_any_eq (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_eq_int:\n\tvceqfs\t%v\[0-9\]*,%v24,%v26\n\tjnle 1 } } */

int __attribute__((noinline,noclone))
any_ne_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_any_ne (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_ne_int:\n\tvceqfs\t%v\[0-9\]*,%v24,%v26\n\tje 1 } } */

int __attribute__((noinline,noclone))
any_gt_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_any_gt (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_gt_int:\n\tvchfs\t%v\[0-9\]*,%v24,%v26\n\tjnle 1 } } */

int __attribute__((noinline,noclone))
any_lt_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_any_lt (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_lt_int:\n\tvchfs\t%v\[0-9\]*,%v26,%v24\n\tjnle 1 } } */

int __attribute__((noinline,noclone))
any_ge_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_any_ge (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_ge_int:\n\tvchfs\t%v\[0-9\]*,%v26,%v24\n\tje 1 } } */

int __attribute__((noinline,noclone))
any_le_int (vector int a, vector int b)
{
  if (__builtin_expect (vec_any_le (a, b), 1))
    foo ();
}
/* { dg-final { scan-assembler-times any_le_int:\n\tvchfs\t%v\[0-9\]*,%v24,%v26\n\tje 1 } } */

