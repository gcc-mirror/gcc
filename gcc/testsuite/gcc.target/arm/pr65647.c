/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6m_ok } */
/* { dg-options "-O3 -w -fpermissive" } */
/* { dg-add-options arm_arch_v6m } */

a, b, c, e, g = &e, h, i = 7, l = 1, m, n, o, q = &m, r, s = &r, u, w = 9, x,
  y = 6, z, t6 = 7, t8, t9 = 1, t11 = 5, t12 = &t8, t13 = 3, t15,
  t16 = &t15;
struct {
  long long f3;
    char f4
} p = {3}

    ,
  t = {4};

struct S1 {
  long long f0;
  short f1;
    long long f2
} d;
long long f = 4073709551613, t7 = 8, t14 = 4073709551610;
j[];
k = j;
v = &d;
*t10 = j;
struct S1 fn1();
struct S1 fn2() {
  signed char t1;
  struct S1 t2;
  long t3 = x;
  short t4 = h;
  short *t5 = &l;
  fn1(t2, w, 1, o);
  if (u) {
    l = q;
    t1 = a < b ?: b;
    z = c >= 2 || t1 << c;
  }
  *t5 = t4 &= t3;
  fn3(y);
}

fn4() {
  t6 = t.f3;
  fn5(k, t7);
}

struct S1 fn1() {
  f = 0;
  for (; i;)
    ;
  t11 = 0;
  t13 = *t10 = t14 || n;
  t9 = t12;
  for (; p.f4;)
    s = t16 <= fn6();
  if (g)
    v = 0;
}
