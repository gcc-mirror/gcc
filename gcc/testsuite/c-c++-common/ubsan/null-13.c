/* { dg-do run } */
/* { dg-options "-fsanitize=null -fno-sanitize-recover=null -w" } */
/* { dg-shouldfail "ubsan" } */

struct S {
  int i;
  long long j;
  long long m;
};
union U {
  int k;
  struct S l;
};

__attribute__((noinline, noclone)) int
foo (struct S s)
{
  return s.i + s.j + s.m;
}

__attribute__((noinline, noclone)) int
bar (union U *u)
{
  foo (u->l);
}

union U v;

int
main (void)
{
  union U *u = 0;
  asm volatile ("" : "+r" (u) : "r" (&v) : "memory");
  return bar (u);
}

/* { dg-output "member access within null pointer of type 'union U'" } */
