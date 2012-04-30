/* { dg-do compile } */
/* { dg-options "-fgnu-tm -ftrack-macro-expansion=0" } */

#define TC	__attribute__((transaction_callable))
#define TU	__attribute__((transaction_unsafe))
#define TP	__attribute__((transaction_pure))
#define TS	__attribute__((transaction_safe))
extern void f1(void) TC;
extern void f2(void) TU;
extern void f3(void) TP;
extern void f4(void) TS;

extern void g1(void) TC TS;	/* { dg-error "previously declared" } */

extern int v1 TP;		/* { dg-warning "ignored" } */

typedef void t1(void) TC;
typedef void (*t2)(void) TC;
typedef int t3 TC;		/* { dg-warning "ignored" } */

typedef void u0(void);
typedef u0 u1 TC;
typedef u1 u2 TP;		/* { dg-error "previously declared" } */
typedef u0 *u3 TS;
typedef u3 u4 TU;		/* { dg-error "previously declared" } */
