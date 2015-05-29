/* PR middle-end/51590 */
/* { dg-do compile } */

struct S { long a, b; };

extern void baz (char *);

static void
bar (struct S *x)
{
  char c[8];
  int i;

  for (i = 0; i < 8; i++)
    c[i] = x->a >> ((7 - i) * 8);

  baz (c);
}

void
foo (const char *x, struct S *y)
{
  struct S d = *y;
  int i;

  for (i = 0; *x; x++)
    i++;

  if (i != 1)
    return;

  bar (&d);
}

