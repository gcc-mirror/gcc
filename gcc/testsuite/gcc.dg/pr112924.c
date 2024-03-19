/* PR tree-optimization/112924 */
/* { dg-do compile } */
/* { dg-options "-O2 -w" } */
/* { dg-additional-options "-msse2" { target i?86-*-* x86_64-*-* } } */

struct S { long a; char b[64]; };
void foo (struct S a);
char c;
int d[3541];

static void
bar (struct S *s, char *p)
{
  unsigned int a = sizeof (d) - sizeof (int) - s->a;
  long c = __builtin_object_size (s, 0);
  for (; a >= 64; a -= 64, p += 4);
    __builtin___memcpy_chk (s, p, a, c);
}

void
baz (void)
{
  struct S s = {};
  bar (&s, &c);
  foo (s);
}
