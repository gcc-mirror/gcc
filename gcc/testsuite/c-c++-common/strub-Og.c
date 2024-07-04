/* { dg-do compile } */
/* { dg-options "-Og -fstrub=strict -fdump-rtl-expand -fno-stack-protector" } */
/* { dg-require-effective-target strub } */

/* At -Og, without -fno-inline, we fully expand enter, but neither update nor
   leave.  */

int __attribute__ ((__strub__)) var;

int f() {
  return var;
}

/* { dg-final { scan-rtl-dump-not "strub_enter" "expand" } } */
/* { dg-final { scan-rtl-dump "strub_update" "expand" } } */
/* { dg-final { scan-rtl-dump "strub_leave" "expand" } } */
/* { dg-final { scan-rtl-dump-not "\[(\]call\[^\n\]*strub_leave.*\n\[(\]code_label" "expand" } } */
