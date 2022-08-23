/* PR tree-optimization/104715 */
/* { dg-do compile } */
/* { dg-options "-Wdangling-pointer" } */

char *
foo (char *p)
{
  {
    char q[61] = "012345678901234567890123456789012345678901234567890123456789";
    char *r = q;
    p = __builtin_strcat (p, r);
  }
  return p;	/* { dg-bogus "using dangling pointer" } */
}

char *
bar (char *p)
{
  {
    char q[] = "0123456789";
    char *r = q;
    p = __builtin_strstr (p, r);
  }
  return p;	/* { dg-bogus "using dangling pointer" } */
}

char *
baz (char *p)
{
  {
    char q[] = "0123456789";
    char *r = q;
    p = __builtin_strpbrk (p, r);
  }
  return p;	/* { dg-bogus "using dangling pointer" } */
}
