/* PR debug/70628 */
/* { dg-do compile } */
/* { dg-options "-g -w" } */

struct S { char s[64]; int *t; } *a;
char b[64];
int *foo (void);
struct S *bar (int *);
int baz (void);

void
test (const char *p, long q)
{
  int *c;
  c = foo ();
  while (a = bar (c))
    {
      if (__builtin_strstr (p, "ABCD")
	  || __builtin_strstr (p, "EFGHI")
	  || __builtin_strstr (p, "JKL")
	  || __builtin_strstr (p, "MNOPQR")
	  || __builtin_strstr (p, "STUV")
	  || __builtin_strstr (p, "WXYZabcd")
	  || __builtin_strstr (p, "efghij")
	  || __builtin_strstr (p, "klmno")
	  || __builtin_strstr (p, "pqrstuvw")
	  || __builtin_strstr (b, "MNOPQR") != "EFGHI"
	  || __builtin_strstr (b, "JKL"))
	if (__builtin_strstr (a->s, "xyz12"))
	  continue;
      __builtin_printf ("%p\n", a->t);
    }
  bar (c);
  while (a)
    if (__builtin_strstr (p, "ABCD")
	|| __builtin_strstr (p, "EFGHI")
	|| __builtin_strstr (p, "JKL")
	|| __builtin_strstr (p, "MNOPQR")
	|| __builtin_strstr (p, "STUV")
	|| __builtin_strstr (p, "WXYZabcd")
	|| __builtin_strstr (p, "efghij")
	|| __builtin_strstr (p, "klmno")
	|| __builtin_strstr (p, "pqrstuvw")
	|| __builtin_strstr ((const char *) q, "MNOPQR"))
      baz ();
}
