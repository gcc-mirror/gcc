/* { dg-do compile { target i?86-*-* } } */
/* { dg-options "-march=generic" } */
/* { dg-error "generic CPU can be used only for -mtune" "" { target *-*-* } 0 } */
/* { dg-bogus "march" "" { target *-*-* } 0 } */
int i;
