/* { dg-do compile } */
/* { dg-options "-Wmemset-elt-size" } */
enum a {
  a_1,
  a_2,
  a_n
};
int t1[20];
int t2[a_n];

struct s
{
  int t[20];
};

void foo (struct s *s)
{
  __builtin_memset (t1, 0, 20); /* { dg-warning "element size" } */
  __builtin_memset (t2, 0, a_n); /* { dg-warning "element size" } */
  __builtin_memset (s->t, 0, 20); /* { dg-warning "element size" } */
}

char u1[20];
char u2[a_n];

struct s2
{
  char u[20];
};

void bar (struct s2 *s)
{
  __builtin_memset (u1, 0, 20);
  __builtin_memset (u2, 0, a_n);
  __builtin_memset (s->u, 0, 20);
}
