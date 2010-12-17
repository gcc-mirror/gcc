/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mtune=*" } { "" } } */
/* { dg-options "-mtune=foo" } */
/* { dg-error "mtune" "" { target *-*-* } 0 } */
/* { dg-bogus "march" "" { target *-*-* } 0 } */
/* { dg-bogus "mcpu" "" { target *-*-* } 0 } */
int i;
