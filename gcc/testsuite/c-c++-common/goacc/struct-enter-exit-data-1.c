/* Check 'GOMP_MAP_STRUCT' mapping, and in particular that it gets removed from
   OpenACC 'exit data' directives.  */

/* { dg-additional-options "-fdump-tree-gimple" } */

struct str {
  int a;
  int *b;
  int *c;
  int d;
  int *e;
  int f;
};

#define N 1024

void
test (int *b, int *c, int *e)
{
  struct str s = { .a = 0, .b = b, .c = c, .d = 0, .e = e, .f = 0 };

#pragma acc enter data copyin(s.a, s.b[0:N], s.c[0:N] /* , s.d */ /* , s.e[0:N] */, s.f)
  /* { dg-final { scan-tree-dump {(?n)#pragma omp target oacc_enter_data map\(struct:s \[len: 4\]\) map\(to:s.a \[len: [0-9]+\]\) map\(alloc:s.b \[len: [0-9]+\]\) map\(alloc:s.c \[len: [0-9]+\]\) map\(to:s.f \[len: [0-9]+\]\) map\(to:\*[_0-9]+ \[len: [0-9]+\]\) map\(attach:s.b \[bias: 0\]\) map\(to:\*[_0-9]+ \[len: [0-9]+\]\) map\(attach:s.c \[bias: 0\]\)$} gimple } } */

#pragma acc exit data copyout(s.a, s.b[0:N], s.c[0:N] /* , s.d */ /* , s.e[0:N] */, s.f)
  /* { dg-final { scan-tree-dump {(?n)#pragma omp target oacc_exit_data map\(from:s.a \[len: [0-9]+\]\) map\(release:s.b \[len: [0-9]+\]\) map\(release:s.c \[len: [0-9]+\]\) map\(from:s.f \[len: [0-9]+\]\) map\(from:\*[_0-9]+ \[len: [0-9]+\]\) map\(detach:s.b \[bias: 0\]\) map\(from:\*[_0-9]+ \[len: [0-9]+\]\) map\(detach:s.c \[bias: 0\]\)$} gimple } } */
}
