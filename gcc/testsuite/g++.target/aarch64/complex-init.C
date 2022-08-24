/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

/*
** _Z1fii:
** ...
** 	bfi	x0, x1, 32, 32
** 	ret
*/
_Complex int f(int a, int b) {
    _Complex int t = a + b * 1i;
    return t;
}

/*
** _Z2f2ii:
** ...
** 	bfi	x0, x1, 32, 32
** 	ret
*/
_Complex int f2(int a, int b) {
    _Complex int t = {a, b};
    return t;
}

/* 
** _Z12f_convolutedii:
** ...
** 	bfi	x0, x1, 32, 32
** 	ret
*/
_Complex int f_convoluted(int a, int b) {
    _Complex int t = (_Complex int)a;
    __imag__ t = b;
    return t;
}
