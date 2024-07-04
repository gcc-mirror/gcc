/* { dg-do compile } */
/* { dg-options "-O2 -fstrub=strict -fdump-rtl-expand" } */
/* { dg-require-effective-target strub } */

/* At -O2, without -fno-inline, we fully expand enter and update, and add a test
   around the leave call.  */

int __attribute__ ((__strub__)) var;

int f() {
  return var;
}

/* { dg-final { scan-rtl-dump-not "strub_enter" "expand" } } */
/* { dg-final { scan-rtl-dump-not "strub_update" "expand" } } */
/* { dg-final { scan-rtl-dump "strub_leave" "expand" } } */
/* { dg-final { scan-rtl-dump "\[(\]call\[^\n\]*strub_leave.*\n\[(\]code_label" "expand" } } */
