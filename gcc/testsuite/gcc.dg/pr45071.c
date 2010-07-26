/* { dg-do compile } */
/* { dg-options "-O -ftree-vrp -ftree-vectorize" } */

struct A
{
  int i;
};

struct B
{
  struct A a;
};

extern void f4 (void *);

inline void
f3 (struct A *a)
{
  f4 (a);
  while (a->i);
}

static inline void
f2 (struct B *b)
{
  f3 (&b->a);
}

void
f1 ()
{
  struct B *b = 0;
  f2 (b);
}
