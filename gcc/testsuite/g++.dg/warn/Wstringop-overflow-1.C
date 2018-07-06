// { dg-do compile }
// { dg-additional-options "-O2 -Wstringop-overflow=2" }

struct S {
    char a[5];
    void (*pf)(void);
};

void f (struct S *s, int n)
{
  if (n < sizeof s->a + 1)
    n = sizeof s->a + 1;

  __builtin_strncpy (s->a, "123456", n);   // { dg-warning "writing 6" }
}
