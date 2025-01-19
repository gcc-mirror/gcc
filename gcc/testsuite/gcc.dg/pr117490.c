/* { dg-do run } */
/* { dg-options "-O2 -std=c23" } */

typedef struct {
  int i1;
} s1;

typedef struct {
  int i1;
} s2_alt;

[[gnu::noinline,gnu::noipa]]
int f2(s1 *s1p, s2_alt *s2p) {
  s1p->i1 = 2;
  s2p->i1 = 3;
  return s1p->i1 * 3;
}

int main()
{
  s1 a;
  if (9 != f2(&a, (void*)&a))
	  __builtin_abort();
}

