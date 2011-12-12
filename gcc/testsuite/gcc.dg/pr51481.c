/* PR tree-optimization/51481 */
/* { dg-do compile } */
/* { dg-options "-O -fexceptions -fipa-cp -fipa-cp-clone" } */

extern const unsigned short int **foo (void)
  __attribute__ ((__nothrow__, __const__));
struct S { unsigned short s1; int s2; };
extern struct S *s[26];

void
bar (int x, struct S *y, ...)
{
  static struct S *t;
  __builtin_va_list ap;
  __builtin_va_start (ap, y);
  if (t != s[7])
    {
      const char *p = "aAbBc";
      t = s[7];
      while ((*foo ())[(unsigned char) *p])
	p++;
    }
  __builtin_printf (x == 0 ? "abc\n" : "def\n");
  if (y != 0)
    __builtin_printf ("ghi %d %d", y->s2, y->s1);
  __builtin_va_end (ap);
}

void
baz (char *x)
{
  bar (1, 0, x);
}
