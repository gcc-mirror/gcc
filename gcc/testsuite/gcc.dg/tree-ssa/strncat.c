/* PR tree-optimization/83075 - Invalid strncpy optimization */
/* { dg-do run } */
/* { dg-options "-O2 -Wstringop-overflow" } */

typedef __SIZE_TYPE__ size_t;

__attribute__((noipa)) size_t
foo (char *p, char *q, size_t *r)
{
  size_t n0 = __builtin_strlen (p);
  __builtin_strncat (q, p, n0);		/* { dg-warning "specified bound depends on the length" } */
  size_t n1 = __builtin_strlen (p);
  *r = n0;
  return n1;
}

int
main ()
{
  char a[8] = "";
  __builtin_strcpy (a, "123");
  size_t n0 = __builtin_strlen (a);
  __builtin_strncat (a + 3, a, n0);	/* { dg-warning "specified bound depends on the length" } */
  size_t n1 = __builtin_strlen (a);
  if (n1 == n0)
    __builtin_abort ();
  a[6] = '7';
  __builtin_strcpy (a, "456");
  size_t n2;
  if (foo (a, a + 3, &n2) != 6 || n2 != 3)
    __builtin_abort ();
  if (__builtin_memcmp (a, "456456\0", sizeof "456456\0"))
    __builtin_abort ();
  return 0;
}
