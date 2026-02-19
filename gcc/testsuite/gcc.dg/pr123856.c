/* { dg-do compile } */
/* { dg-options "-Wenum-conversion" } */

typedef __attribute__((__hardbool__)) int A, B;

B b;
void bar(A) { }
void foo() { bar(b); }		/* { dg-warning "implicit conversion" } */

void bar2(__attribute__((__hardbool__)) int)  { }
void foo2() { bar2(b); }	/* { dg-warning "implicit conversion" } */

__attribute__((__hardbool__)) int c;
void bar3(__attribute__((__hardbool__)) int)  { }
void foo3() { bar2(c); }	/* { dg-warning "implicit conversion" } */

void bar4(int)  { }
void foo4() { bar2(c); }	/* { dg-warning "implicit conversion" } */

