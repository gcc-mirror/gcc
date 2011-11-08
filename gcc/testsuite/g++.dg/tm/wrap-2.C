/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

#define W(X)	__attribute__((transaction_wrap(X)))
void f1(void);
void f2(int);
int i3;
int f7(void);

void g1(void) W(f1);
void g2(void) W(f2);	/* { dg-error "is not compatible" } */
void g3(void) W(i3);	/* { dg-error "is not a function" } */
void g4(void) W(f4);	/* { dg-error "not declared in this scope\|not an identifier" } */
void g5(void) W(1);	/* { dg-error "not an identifier" } */
void g6(void) W("f1");	/* { dg-error "not an identifier" } */
void g7(void) W(f7);	/* { dg-error "is not compatible" } */
