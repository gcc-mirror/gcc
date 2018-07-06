/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "" } } */
/* { dg-options "-march=generic -fcf-protection=none" } */
/* { dg-error "'generic' CPU can be used only for '-mtune=' switch" "" { target *-*-* } 0 } */
/* { dg-bogus "march" "" { target *-*-* } 0 } */
int i;
