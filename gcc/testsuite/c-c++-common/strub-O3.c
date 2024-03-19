/* { dg-do compile } */
/* { dg-options "-O3 -fstrub=strict -fdump-rtl-expand" } */
/* { dg-require-effective-target strub } */

int __attribute__ ((__strub__)) var;

int f() {
  return var;
}

/* { dg-final { scan-rtl-dump-not "strub_enter" "expand" } } */
/* { dg-final { scan-rtl-dump-not "strub_update" "expand" } } */
/* { dg-final { scan-rtl-dump-not "strub_leave" "expand" } } */
