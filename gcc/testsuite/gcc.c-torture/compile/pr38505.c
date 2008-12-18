/* PR middle-end/38505 */
/* { dg-do compile } */

struct S
{
  unsigned short a[50];
  unsigned short b[20];
};
extern void bar (struct S *);
extern void baz (unsigned short *);
extern unsigned short d[];

void
foo (void)
{
  struct S s;
  unsigned short g[50];

  baz (g);
  __builtin_memcpy (&s, g, sizeof (g));
  __builtin_memcpy (s.b, d, sizeof (s.b));
  bar (&s);
}
