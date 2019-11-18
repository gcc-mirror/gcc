/* PR middle-end/92493 - ICE in get_origin_and_offset at gimple-ssa-sprintf.c
   { dg-do compile }
   { dg-options "-O3 -Wall" } */

struct A
{
  int i;
  char a[2];
} *p;

struct B
{
  short j;
  struct A a;
} b;

void warn (int j)
{
  struct A *q = &b.a;
  p = q + j;
  __builtin_snprintf (p->a, 8, "%s", p->a);   // { dg-warning "\\\[-Wrestrict" }
}

void nowarn (char *d, int j)
{
  struct A *q = &b.a;
  p = q + j;
  __builtin_snprintf (d, 8, "%s", p->a);
}
