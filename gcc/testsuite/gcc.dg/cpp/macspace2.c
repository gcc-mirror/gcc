/* PR preprocessor/19475 */
/* { dg-do preprocess } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

#define a!		/* { dg-error "requires whitespace" } */
#define b"		/* { dg-error "requires whitespace" } */
/* { dg-error "missing terminating" "missing-terminating" { target *-*-* } .-1 } */
#define c#		/* { dg-error "requires whitespace" } */
#define d%		/* { dg-error "requires whitespace" } */
#define e&		/* { dg-error "requires whitespace" } */
#define f'		/* { dg-error "requires whitespace" } */
/* { dg-error "missing terminating" "missing-terminating" { target *-*-* } .-1 } */
#define g)		/* { dg-error "requires whitespace" } */
#define h*		/* { dg-error "requires whitespace" } */
#define i+		/* { dg-error "requires whitespace" } */
#define j,		/* { dg-error "requires whitespace" } */
#define k-		/* { dg-error "requires whitespace" } */
#define l.		/* { dg-error "requires whitespace" } */
#define m/		/* { dg-error "requires whitespace" } */
#define n:		/* { dg-error "requires whitespace" } */
#define o;		/* { dg-error "requires whitespace" } */
#define p<		/* { dg-error "requires whitespace" } */
#define q=		/* { dg-error "requires whitespace" } */
#define r>		/* { dg-error "requires whitespace" } */
#define s?		/* { dg-error "requires whitespace" } */
#define t[		/* { dg-error "requires whitespace" } */
#define u]		/* { dg-error "requires whitespace" } */
#define v^		/* { dg-error "requires whitespace" } */
#define w{		/* { dg-error "requires whitespace" } */
#define x|		/* { dg-error "requires whitespace" } */
#define y}		/* { dg-error "requires whitespace" } */
#define z~		/* { dg-error "requires whitespace" } */
#define A>>		/* { dg-error "requires whitespace" } */
#define B<<		/* { dg-error "requires whitespace" } */
#define E&&		/* { dg-error "requires whitespace" } */
#define F||		/* { dg-error "requires whitespace" } */
#define G==		/* { dg-error "requires whitespace" } */
#define H!=		/* { dg-error "requires whitespace" } */
#define I>=		/* { dg-error "requires whitespace" } */
#define J<=		/* { dg-error "requires whitespace" } */
#define K+=		/* { dg-error "requires whitespace" } */
#define L-=		/* { dg-error "requires whitespace" } */
#define M*=		/* { dg-error "requires whitespace" } */
#define N/=		/* { dg-error "requires whitespace" } */
#define O%=		/* { dg-error "requires whitespace" } */
#define P&=		/* { dg-error "requires whitespace" } */
#define Q|=		/* { dg-error "requires whitespace" } */
#define R^=		/* { dg-error "requires whitespace" } */
#define S>>=		/* { dg-error "requires whitespace" } */
#define T<<=		/* { dg-error "requires whitespace" } */
#define W...		/* { dg-error "requires whitespace" } */
#define X++		/* { dg-error "requires whitespace" } */
#define Y--		/* { dg-error "requires whitespace" } */
#define Z->		/* { dg-error "requires whitespace" } */
#define aa::		/* { dg-error "requires whitespace" } */
#define ab->*		/* { dg-error "requires whitespace" } */
#define ac.*		/* { dg-error "requires whitespace" } */
#define ad\x		/* { dg-error "requires whitespace" } */
#define ae\\x		/* { dg-error "requires whitespace" } */
#define af'1'		/* { dg-error "requires whitespace" } */
#define ag"abc"		/* { dg-error "requires whitespace" } */

int dummy;
