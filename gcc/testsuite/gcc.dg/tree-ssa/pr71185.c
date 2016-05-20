/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-options "-O3 -march=barcelona" { target x86_64-*-* i?86-*-* } } */

union U { struct S { int l; int m; } p; long long a; } b;
int a, c;

void
foo ()
{
  for (; b.p.m; b.a += c)
    a = b.p.l / 65536.0 * 65536.0;
}
