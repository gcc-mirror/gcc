/* { dg-do compile } */
/* { dg-options "-O0 -fstrub=strict -fdump-rtl-expand" } */
/* { dg-require-effective-target strub } */

/* At -O0, none of the strub builtins are expanded inline.  */

int __attribute__ ((__strub__)) var;

int f() {
  return var;
}

/* { dg-final { scan-rtl-dump "strub_enter" "expand" } } */
/* { dg-final { scan-rtl-dump "strub_update" "expand" } } */
/* { dg-final { scan-rtl-dump "strub_leave" "expand" } } */
