/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */

__extension__ typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern void *memset (void *, int, size_t);

struct A
{
  char pad[48];
  int i;
  int pad2;
  int j;
};
__thread struct A a;

int *
__attribute__((noinline))
foo (void)
{
  return &a.i;
}

int
main (void)
{
  int *p = foo ();
  memset (&a, 0, sizeof (a));
  a.i = 6;
  a.j = 8;
  if (p[0] != 6 || p[1] != 0 || p[2] != 8)
    abort ();
  return 0;
}
