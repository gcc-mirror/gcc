/* { dg-do compile } */
/* { dg-options "-fno-common" } */

/* COFF does not support weak, and dg doesn't support UNSUPPORTED.  */
/* { dg-do compile { xfail *-*-coff i?86-pc-cygwin h8300-*-hms } } */

/* { dg-final { global target_triplet } } */
/* { dg-final { if [string match h8300-*-hms $target_triplet ] {return} } } */
/* { dg-final { if [string match i?86-pc-cygwin $target_triplet ] {return} } } */
/* { dg-final { if [string match *-*-coff $target_triplet ] {return} } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?a" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?b" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?c" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?d" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?e" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?g" } } */
/* { dg-final { scan-assembler-not "weak\[^ \t\]*\[ \t\]_?i" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?j" } } */

#pragma weak a
int a;

int b;
#pragma weak b

#pragma weak c
extern int c;
int c;

extern int d;
#pragma weak d
int d;

#pragma weak e
void e(void) { }

#if 0
/* This permutation is illegal.  */
void f(void) { }
#pragma weak f
#endif

#pragma weak g
int g = 1;

#if 0
/* This permutation is illegal.  */
int h = 1;
#pragma weak h
#endif

#pragma weak i
extern int i;

#pragma weak j
extern int j;
int use_j() { return j; }
