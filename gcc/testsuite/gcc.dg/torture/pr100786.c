/* { dg-do compile } */
/* { dg-require-alias "" } */

const double a = 0;
extern int b __attribute__((alias("a")));
void inc() { b++; }

const int a2 = 0;
extern double b2 __attribute__((alias("a2")));
void inc2() { b2+=1; }
