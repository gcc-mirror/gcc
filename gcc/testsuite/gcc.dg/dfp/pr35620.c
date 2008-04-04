/* { dg-do compile } */
/* { dg-options "-std=gnu99 -O2" } */

extern void foo (_Decimal32);
_Decimal32 *p;

extern int i;
union { _Decimal32 a; int b; } u;

void
blatz (void)
{
  _Decimal32 d;
  u.b = i;
  d = u.a;
  foo (d);
}

void
bar (void)
{
  foo (*p);
}
