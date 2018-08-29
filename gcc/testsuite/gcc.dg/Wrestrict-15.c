/* PR 85365 - -Wrestrict false positives with -fsanitize=undefined
   { dg-do compile }
   { dg-options "-O2 -Wrestrict -fsanitize=undefined" } */

typedef __SIZE_TYPE__ size_t;

char *strcpy (char *, const char *);
char *strcat (char *, const char *);

size_t strlen (char *);

extern char a[], b[], d[];

size_t t1 (char *g, int i)
{
  /* The following exercises the handling in gimple-fold.c.  */
  strcpy (g + 4, i ? b : a);    /* { dg-bogus "\\\[-Wrestrict]" } */
  return strlen (g + 4);
}

void t2 (char *g, int i)
{
  strcpy (g + 4, i ? b : a);    /* { dg-bogus "\\\[-Wrestrict]" } */
  strcat (g + 4, d);
}

void t3 (char *g, int i)
{
  /* The following exercises the handling in gimple-ssa-warn-restrict.c.  */
  strcat (g + 4, i ? b : a);    /* { dg-bogus "\\\[-Wrestrict]" } */
  strcat (g + 4, d);
}

void t4 (char *p, char *q)
{
  strcpy (p, q);                /* { dg-bogus "\\\[-Wrestrict]" } */
  strcat (p, q + 32);
}
