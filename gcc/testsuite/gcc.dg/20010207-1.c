/* { dg-do compile { target i?86-*-* } } */
/* { dg-options "-O3 -gdwarf-2" } */

extern inline int f1 (void) {return f2();}
int f3 (void) {return f1();}
int f1 (void) {return 0;}
