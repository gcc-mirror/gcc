/* { dg-do run { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Test for correct handling of AltiVec constants passed
   through '...' (va_arg).  */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include <altivec.h>
#include "altivec_check.h"

#define CHECK_INVARIANT(expr) \
  if (!(expr)) { \
    printf ("ASSERT FAILED: %d: %s\n", __LINE__, #expr); \
    abort (); \
  }

struct foo { int x; int y; };
struct vfoo { int x; __vector signed int v; int y; };
union u { __vector signed int v; signed int i[4]; };

struct foo x_g = { 3, 4};
struct vfoo vx_g = { 10, {11, 12, 13, 14}, 15 };
__vector signed int v_g = {22, 23, 24, 25};
struct vfoo vx2_g = { 30, {31, 32, 33, 34}, 35 };
__vector signed int v2_g = {40, 41, 42, 43};
int i_1 = 99, i_2 = 33;
double d_2 = 1.5, d_3 = 1.75;
double ld_1 = 1.25;

void bar (int i, ... )
{
    struct foo xi;
    double d;
    double ld;
    float f;
    char c;
    short s;
    va_list ap;
    va_start(ap, i);
    xi = va_arg(ap, struct foo);
    s = (short)va_arg(ap, int);
    f = (float)va_arg(ap, double);
    ld = va_arg(ap, double);
    c = (char)va_arg(ap, int);
    d = va_arg(ap, double);
    va_end(ap);
    
    CHECK_INVARIANT (xi.x == x_g.x && xi.y == x_g.y);
    CHECK_INVARIANT (s == (short)i_2);
    CHECK_INVARIANT (f == (float)d_2);
    CHECK_INVARIANT (ld == ld_1);
    CHECK_INVARIANT (c == (char)i_1);
    CHECK_INVARIANT (d == d_3);
}

void baz (int i, ... )
{
    struct vfoo vx, vx2;
    __vector signed int v_i, v2_i;
    int j, k, l;
    va_list ap;
    va_start(ap, i);
    v_i = va_arg(ap, __vector signed int); 
    j = va_arg(ap, int);
    vx = va_arg(ap, struct vfoo);
    k = va_arg(ap, int);
    v2_i = va_arg(ap, __vector signed int);
    l = va_arg(ap, int);
    vx2 = va_arg(ap, struct vfoo);
    va_end(ap);
      
    CHECK_INVARIANT (vec_all_eq (v_i, v_g));
    CHECK_INVARIANT (j == i_1);
    CHECK_INVARIANT (vx.x == vx_g.x);
    CHECK_INVARIANT (vec_all_eq (vx.v, vx_g.v));
    CHECK_INVARIANT (vx.y == vx_g.y);
    CHECK_INVARIANT (k == i_1);
    CHECK_INVARIANT (vec_all_eq (v2_i, v2_g));
    CHECK_INVARIANT (l == i_1);
    CHECK_INVARIANT (vx2.x == vx2_g.x);
    CHECK_INVARIANT (vec_all_eq (vx2.v, vx2_g.v));
    CHECK_INVARIANT (vx2.y == vx2_g.y);
}

void quux (int i, ... )
{
    __vector signed int v_i, v2_i;
    union u vi, v2i;
    va_list ap;
    va_start(ap, i);
    v_i = va_arg(ap, __vector signed int);
    v2_i = va_arg(ap, __vector signed int);
    va_end(ap);
    vi.v = v_i;
    v2i.v = v2_i;

    CHECK_INVARIANT (vec_all_eq (v_i, v_g));
    CHECK_INVARIANT (vec_all_eq (v2_i, v_g));
    CHECK_INVARIANT (vec_all_eq (vi.v, v_g));
    CHECK_INVARIANT (vec_all_eq (v2i.v, v_g));
}

void baz2 (int i, ... )
{
    struct vfoo vx;
    union u vxi;
    va_list ap;
    va_start(ap, i);
    vx = va_arg(ap, struct vfoo);
    va_end(ap);
    vxi.v = vx.v;

    CHECK_INVARIANT (vx.x == vx_g.x);
    CHECK_INVARIANT (vec_all_eq (vx.v, vx_g.v));
    CHECK_INVARIANT (vx.y == vx_g.y);
    CHECK_INVARIANT (vec_all_eq (vxi.v, vx_g.v));
}

void main1(void)
{
    CHECK_INVARIANT (sizeof(struct foo) == 8 && sizeof(struct vfoo) == 48);

    bar(i_1, x_g, (short)i_2, (float)d_2, ld_1, (char)i_1, d_3);
    baz(i_1, v_g, i_1, vx_g, i_1, v2_g, i_1, vx2_g); 
    quux(i_1, v_g, v_g);
    baz2(i_1, vx_g);
}

int main(void)
{
    altivec_check();
    main1();
    return 0;
}
