/* PR tree-optimization/97053 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-dse" } */

struct __attribute__((packed, may_alias)) S { long long s; };
struct __attribute__((packed, may_alias)) T { short t; };

__attribute__((noipa)) void
test (char *p, char *q, int s)
{
  if ((s & 1) == 0)
    {
      if (*(short __attribute__((may_alias)) *) &p[sizeof (short)]
	  != *(short __attribute__((may_alias)) *) &q[sizeof (short)]
	  || (((struct S __attribute__((may_alias)) *) &p[1])->s
	      != ((struct S __attribute__((may_alias)) *) &q[1])->s)
	  || (*(short __attribute__((may_alias)) *) &p[2 * sizeof (short)]
	      != *(short __attribute__((may_alias)) *) &q[2 * sizeof (short)]))
	__builtin_abort ();
    }
  else
    {
      if (*(short __attribute__((may_alias)) *) &p[sizeof (short)]
	  != *(short __attribute__((may_alias)) *) &q[sizeof (short)]
	  || (((struct S __attribute__((may_alias)) *) &p[1])->s
	      != ((struct S __attribute__((may_alias)) *) &q[1])->s)
	  || (((struct T __attribute__((may_alias)) *) &p[2 * sizeof (short) - 1])->t
	      != ((struct T __attribute__((may_alias)) *) &q[2 * sizeof (short) - 1])->t)
	  || p[3 * sizeof (short) - 2] != q[3 * sizeof (short) - 2])
	__builtin_abort ();
    }
}

__attribute__((noipa)) void
foo (long long *p, char *q, char *r, char *s)
{
  char a[64] __attribute__((aligned (__alignof (short))));
  *(short __attribute__((may_alias)) *) &a[sizeof (short)] = 1;
  ((struct S __attribute__((may_alias)) *) &a[1])->s = p[0];
  *(short __attribute__((may_alias)) *) &a[2 * sizeof (short)] = 2;
  *(short __attribute__((may_alias)) *) &q[sizeof (short)] = 1;
  ((struct S __attribute__((may_alias)) *) &r[1])->s = p[0];
  *(short __attribute__((may_alias)) *) &s[2 * sizeof (short)] = 2;
  test (a, q, 0);
}

__attribute__((noipa)) void
bar (long long *p, char *q, char *r, char *s, char *t)
{
  char a[64] __attribute__((aligned (__alignof (short))));
  *(short __attribute__((may_alias)) *) &a[sizeof (short)] = 1;
  ((struct S __attribute__((may_alias)) *) &a[1])->s = p[0];
  ((struct T __attribute__((may_alias)) *) &a[2 * sizeof (short) - 1])->t = 2;
  a[3 * sizeof (short) - 2] = 3;
  *(short __attribute__((may_alias)) *) &q[sizeof (short)] = 1;
  ((struct S __attribute__((may_alias)) *) &r[1])->s = p[0];
  ((struct T __attribute__((may_alias)) *) &s[2 * sizeof (short) - 1])->t = 2;
  t[3 * sizeof (short) - 2] = 3;
  test (a, q, 1);
}

__attribute__((noipa)) void
baz (long long *p, char *q, char *r, char *s)
{
  char a[64] __attribute__((aligned (__alignof (short))));
  *(short __attribute__((may_alias)) *) &a[2 * sizeof (short)] = 2;
  ((struct S __attribute__((may_alias)) *) &a[1])->s = p[0];
  *(short __attribute__((may_alias)) *) &a[sizeof (short)] = 1;
  *(short __attribute__((may_alias)) *) &q[2 * sizeof (short)] = 2;
  ((struct S __attribute__((may_alias)) *) &r[1])->s = p[0];
  *(short __attribute__((may_alias)) *) &s[sizeof (short)] = 1;
  test (a, q, 2);
}

__attribute__((noipa)) void
qux (long long *p, char *q, char *r, char *s, char *t)
{
  char a[64] __attribute__((aligned (__alignof (short))));
  *(short __attribute__((may_alias)) *) &a[2 * sizeof (short) - 1] = 2;
  ((struct S __attribute__((may_alias)) *) &a[1])->s = p[0];
  a[3 * sizeof (short) - 2] = 3;
  *(short __attribute__((may_alias)) *) &a[sizeof (short)] = 1;
  ((struct T __attribute__((may_alias)) *) &q[2 * sizeof (short) - 1])->t = 2;
  ((struct S __attribute__((may_alias)) *) &r[1])->s = p[0];
  s[3 * sizeof (short) - 2] = 3;
  ((struct T __attribute__((may_alias)) *) &t[sizeof (short)])->t = 1;
  test (a, q, 3);
}

__attribute__((noipa)) void
corge (long long *p, char *q, char *r, char *s, short u[3])
{
  char a[64] __attribute__((aligned (__alignof (short))));
  *(short __attribute__((may_alias)) *) &a[2 * sizeof (short)] = u[2];
  ((struct S __attribute__((may_alias)) *) &a[1])->s = p[0];
  *(short __attribute__((may_alias)) *) &a[sizeof (short)] = u[1];
  *(short __attribute__((may_alias)) *) &q[2 * sizeof (short)] = u[2];
  ((struct S __attribute__((may_alias)) *) &r[1])->s = p[0];
  *(short __attribute__((may_alias)) *) &s[sizeof (short)] = u[1];
  test (a, q, 4);
}

__attribute__((noipa)) void
garply (long long *p, char *q, char *r, char *s, short u[3])
{
  char a[64] __attribute__((aligned (__alignof (short))));
  *(short __attribute__((may_alias)) *) &a[sizeof (short)] = u[1];
  ((struct S __attribute__((may_alias)) *) &a[1])->s = p[0];
  *(short __attribute__((may_alias)) *) &a[2 * sizeof (short)] = u[2];
  *(short __attribute__((may_alias)) *) &s[sizeof (short)] = u[1];
  ((struct S __attribute__((may_alias)) *) &r[1])->s = p[0];
  *(short __attribute__((may_alias)) *) &q[2 * sizeof (short)] = u[2];
  test (a, q, 6);
}

int
main ()
{
  char a[64] __attribute__((aligned (__alignof (short))));
  long long p = -1LL;
  short u[] = { 1, 2, 3 };
  foo (&p, &a[0], &a[0], &a[0]);
  bar (&p, &a[0], &a[0], &a[0], &a[0]);
  baz (&p, &a[0], &a[0], &a[0]);
  qux (&p, &a[0], &a[0], &a[0], &a[0]);
  corge (&p, &a[0], &a[0], &a[0], u);
  garply (&p, &a[0], &a[0], &a[0], u);
  return 0;
}
