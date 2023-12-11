/* { dg-do compile } */
/* { dg-options "-Os -fstrub=strict -fdump-rtl-expand -fno-stack-protector" } */
/* { dg-require-effective-target strub } */

/* At -Os, without -fno-inline, we fully expand enter, and also update.  The
   expanded update might be larger than a call proper, but argument saving and
   restoring required by the call will most often make it larger.  The leave
   call is left untouched.  */

int __attribute__ ((__strub__)) var;

int f() {
  return var;
}

/* { dg-final { scan-rtl-dump-not "strub_enter" "expand" } } */
/* { dg-final { scan-rtl-dump-not "strub_update" "expand" } } */
/* { dg-final { scan-rtl-dump "strub_leave" "expand" } } */
/* { dg-final { scan-rtl-dump-not "\[(\]call\[^\n\]*strub_leave.*\n\[(\]code_label" "expand" } } */
