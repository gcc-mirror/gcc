/* PR c++/21210 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef float __complex__ fcomplex;
fcomplex cplx = fcomplex(0);
