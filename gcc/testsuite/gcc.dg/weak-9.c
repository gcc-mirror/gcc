/* { dg-do compile } */
/* { dg-options "-fno-common" } */

/* COFF does not support weak, and dg doesn't support UNSUPPORTED.  */
/* { dg-do compile { xfail *-*-coff i?86-pc-cygwin h8300-*-hms } } */

/* { dg-final { global target_triplet } } */
/* { dg-final { if [string match h8300-*-hms $target_triplet ] {return} } } */
/* { dg-final { if [string match i?86-pc-cygwin $target_triplet ] {return} } } *
/
/* { dg-final { if [string match *-*-coff $target_triplet ] {return} } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f1" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f2" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f3" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f4" } } */
/* { dg-final { scan-assembler "notf1" } } */
/* { dg-final { scan-assembler "notf2" } } */
/* { dg-final { scan-assembler "notf3" } } */
/* { dg-final { scan-assembler "notf4" } } */

void f1() __attribute__((weak, alias("notf1")));
void f2() __attribute__((alias("notf2"), weak));

#pragma weak f3=notf3
void f3();

void f4();
#pragma weak f4=notf4
