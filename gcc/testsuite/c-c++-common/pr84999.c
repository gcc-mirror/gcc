/* PR c/84999 */
/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "" } */

typedef __float128 V __attribute__ ((__vector_size__ (2 * sizeof (__float128))));
V a;
typeof (a != 0) b;	/* { dg-error "could not find an integer type of the same size as" "" { target ia32 } } */
typeof (a == 0) c;	/* { dg-error "could not find an integer type of the same size as" "" { target ia32 } } */
typeof (a < 0) d;	/* { dg-error "could not find an integer type of the same size as" "" { target ia32 } } */
typeof (a <= 0) e;	/* { dg-error "could not find an integer type of the same size as" "" { target ia32 } } */
typeof (a > 0) f;	/* { dg-error "could not find an integer type of the same size as" "" { target ia32 } } */
typeof (a >= 0) g;	/* { dg-error "could not find an integer type of the same size as" "" { target ia32 } } */
