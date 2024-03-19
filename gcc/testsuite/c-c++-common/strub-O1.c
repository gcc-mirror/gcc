/* { dg-do compile } */
/* { dg-options "-O1 -fstrub=strict -fdump-rtl-expand" } */
/* { dg-require-effective-target strub } */

/* At -O1, without -fno-inline, we fully expand enter, but neither update nor
   leave.  */

int __attribute__ ((__strub__)) var;

int f() {
  return var;
}

/* { dg-final { scan-rtl-dump-not "strub_enter" "expand" } } */
/* { dg-final { scan-rtl-dump "strub_update" "expand" } } */
/* { dg-final { scan-rtl-dump "strub_leave" "expand" } } */
