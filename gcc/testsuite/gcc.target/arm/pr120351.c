/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */
/* { dg-additional-options "-O2" } */


typedef struct A
{
  int f1;
} A;

__inline void ref (A* x)
{
  __atomic_fetch_add(&x->f1, 1, 0);
}

typedef struct B
{
  A *d;
  int *ptr;
} B;

void insertOne (B*, B*);

void init (B *);
__inline void copy (B *p, B *q)
{
  p->d  = q->d;
  p->ptr = q->ptr;
  ref (p->d);
}

__inline void emplace(B* x)
{
  B dummy;
  B _tmp;
  init (&dummy);
  copy (&_tmp, &dummy);
  insertOne(x, &_tmp);
}

void testing ()
{
  B test;
  init (&test);
  emplace(&test);
}
