/* { dg-do compile } */
/* { dg-options "-O2 -fstrub=strict -fdump-rtl-expand -fno-inline -fno-stack-protector" } */
/* { dg-require-effective-target strub } */

/* With -fno-inline, none of the strub builtins are inlined.  */

int __attribute__ ((__strub__)) var;

int f() {
  return var;
}

/* { dg-final { scan-rtl-dump "strub_enter" "expand" } } */
/* { dg-final { scan-rtl-dump "strub_update" "expand" } } */
/* { dg-final { scan-rtl-dump "strub_leave" "expand" } } */
/* { dg-final { scan-rtl-dump-not "\[(\]call\[^\n\]*strub_leave.*\n\[(\]code_label" "expand" } } */
