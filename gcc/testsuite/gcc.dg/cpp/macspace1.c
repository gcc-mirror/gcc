/* PR preprocessor/19475 */
/* { dg-do preprocess } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors -fno-show-column" } */

#define a!		/* { dg-warning "missing whitespace" } */
#define b"		/* { dg-warning "missing whitespace" } */
#define c#		/* { dg-warning "missing whitespace" } */
#define d%		/* { dg-warning "missing whitespace" } */
#define e&		/* { dg-warning "missing whitespace" } */
#define f'		/* { dg-warning "missing whitespace" } */
#define g)		/* { dg-warning "missing whitespace" } */
#define h*		/* { dg-warning "missing whitespace" } */
#define i+		/* { dg-warning "missing whitespace" } */
#define j,		/* { dg-warning "missing whitespace" } */
#define k-		/* { dg-warning "missing whitespace" } */
#define l.		/* { dg-warning "missing whitespace" } */
#define m/		/* { dg-warning "missing whitespace" } */
#define n:		/* { dg-warning "missing whitespace" } */
#define o;		/* { dg-warning "missing whitespace" } */
#define p<		/* { dg-warning "missing whitespace" } */
#define q=		/* { dg-warning "missing whitespace" } */
#define r>		/* { dg-warning "missing whitespace" } */
#define s?		/* { dg-warning "missing whitespace" } */
#define t[		/* { dg-warning "missing whitespace" } */
#define u]		/* { dg-warning "missing whitespace" } */
#define v^		/* { dg-warning "missing whitespace" } */
#define w{		/* { dg-warning "missing whitespace" } */
#define x|		/* { dg-warning "missing whitespace" } */
#define y}		/* { dg-warning "missing whitespace" } */
#define z~		/* { dg-warning "missing whitespace" } */
#define A>>		/* { dg-warning "missing whitespace" } */
#define B<<		/* { dg-warning "missing whitespace" } */
#define E&&		/* { dg-warning "missing whitespace" } */
#define F||		/* { dg-warning "missing whitespace" } */
#define G==		/* { dg-warning "missing whitespace" } */
#define H!=		/* { dg-warning "missing whitespace" } */
#define I>=		/* { dg-warning "missing whitespace" } */
#define J<=		/* { dg-warning "missing whitespace" } */
#define K+=		/* { dg-warning "missing whitespace" } */
#define L-=		/* { dg-warning "missing whitespace" } */
#define M*=		/* { dg-warning "missing whitespace" } */
#define N/=		/* { dg-warning "missing whitespace" } */
#define O%=		/* { dg-warning "missing whitespace" } */
#define P&=		/* { dg-warning "missing whitespace" } */
#define Q|=		/* { dg-warning "missing whitespace" } */
#define R^=		/* { dg-warning "missing whitespace" } */
#define S>>=		/* { dg-warning "missing whitespace" } */
#define T<<=		/* { dg-warning "missing whitespace" } */
#define W...		/* { dg-warning "missing whitespace" } */
#define X++		/* { dg-warning "missing whitespace" } */
#define Y--		/* { dg-warning "missing whitespace" } */
#define Z->		/* { dg-warning "missing whitespace" } */
#define aa::		/* { dg-warning "missing whitespace" } */
#define ab->*		/* { dg-warning "missing whitespace" } */
#define ac.*		/* { dg-warning "missing whitespace" } */
#define ad\x		/* { dg-warning "missing whitespace" } */
#define ae\\x		/* { dg-warning "missing whitespace" } */
#define af'1'		/* { dg-warning "missing whitespace" } */
#define ag"abc"		/* { dg-warning "missing whitespace" } */

int dummy;
/* { dg-error "missing terminating" "" { target *-*-* } 6 } */
/* { dg-error "missing terminating" "" { target *-*-* } 10 } */
