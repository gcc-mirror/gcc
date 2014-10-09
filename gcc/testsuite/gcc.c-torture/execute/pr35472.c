extern void abort (void);
extern void *memset (void *s, int c, __SIZE_TYPE__ n);
struct S { int i[16]; };
struct S *p;
void __attribute__((noinline,noclone))
foo(struct S *a, struct S *b) { a->i[0] = -1; p = b; }
void test (void)
{
  struct S a, b;
  memset (&a.i[0], '\0', sizeof (a.i));
  memset (&b.i[0], '\0', sizeof (b.i));
  foo (&a, &b);
  *p = a;
  *p = b;
  if (b.i[0] != -1)
    abort ();
}
int main()
{
  test();
  return 0;
}
