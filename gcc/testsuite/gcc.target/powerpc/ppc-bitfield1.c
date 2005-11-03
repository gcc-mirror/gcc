/* { dg-do compile { target powerpc64-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-not "rlwinm \[0-9\]+,\[0-9\]+,\[0-9\]+,1,31"  } } */
/* { dg-final { scan-assembler-not "rlwinm \[0-9\]+,\[0-9\]+,\[0-9\]+,0xffffffff" } } */

/* Origin:Pete Steinmetz <steinmtz@us.ibm.com> */

/* PR 17104 many sign extends added.  */

struct {
 int f1 : 1;
 int f2 : 1;
 int f3 : 1;
 int f4 : 1;
 int f5 : 1;
 int f6 : 1;
 int f7 : 1;
 int f8 : 1;
 int f9 : 1;
 int f10 : 1;
 int f11 : 1;
 int f12 : 1;
 int f13 : 1;
 int f14 : 1;
 int f15 : 1;
 int f16 : 1;
 int f17 : 2;
 int f18 : 2;
 int f19 : 2;
 int f20 : 2;
 int f21 : 2;
 int f22 : 2;
 int f23 : 2;
 int f24 : 2;
 } s;

void foo ()
{

    s.f1 = 0;
    s.f2 = 0;
    s.f3 = 0;
    s.f4 = 0;
    s.f5 = 0;
    s.f6 = 0;
    s.f7 = 0;
    s.f8 = 0;
    s.f9 = 0;
    s.f10 = 0;
    s.f11 = 0;
    s.f12 = 0;
    s.f13 = 0;
    s.f14 = 0;
    s.f15 = 0;
    s.f16 = 0;
    s.f17 = 0;
    s.f18 = 0;
    s.f19 = 0;
    s.f20 = 0;
    s.f21 = 0;
    s.f22 = 0;
    s.f23 = 0;
    s.f24 = 0;

}

