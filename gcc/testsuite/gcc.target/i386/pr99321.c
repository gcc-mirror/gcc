/* PR target/99321 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=btver2 -fno-tree-dce -mavx512vl -mno-avx512bw" } */

typedef unsigned __attribute__((__vector_size__ (8))) A;
typedef unsigned int __attribute__((__vector_size__ (8))) B;
typedef unsigned char __attribute__((__vector_size__ (16))) C;
typedef unsigned __attribute__((__vector_size__ (16))) D;
typedef unsigned int __attribute__((__vector_size__ (16))) E;
typedef unsigned __attribute__((__vector_size__ (16))) F;
typedef unsigned __attribute__((__vector_size__ (32))) G;
typedef int __attribute__((__vector_size__ (32))) H;
typedef unsigned int __attribute__((__vector_size__ (32))) I;
typedef char __attribute__((__vector_size__ (64))) J;
typedef unsigned int __attribute__((__vector_size__ (64))) K;
typedef unsigned long long __attribute__((__vector_size__ (64))) L;
unsigned char a;
unsigned b, c;
H d;
E e, f;
D g;
L h;

A
foo0 (A i, C j, G k, B l, K m, B n, I o)
{
  J p, q = a != p;
  F r = b << f;
  int s = a * 15;
  C t = (1 << ((C) ((C) { 80 } >=j) & sizeof (0)) | (j ^ (C) { 5 }) << (j & sizeof (0))) != 0;
  L u = h;
  H v = d - 40;
  u ^= -(long long) n;
  D w = (char) s > g;
  o ^= c / o;
  J x = p + q + (J) m + (J) u + (J) u;
  G y = ((union { J a; G b;}) x).b + ((union { J a; G b[2];}) x).b[1] + k + v + o;
  C z = ((union { G a; C b;}) y).b + ((union { G a; C b;}) y).b + j + t + (C) g + (C) w + (C) e + (C) f + (C) r;
  A zz = ((union { C a; A b;}) z).b + i + l + n;
  return zz;
}
