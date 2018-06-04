/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

extern void abort (void);

struct S1 {
  unsigned int flag : 1;
  unsigned int size : 31;
};

void foo1 (struct S1 *s, struct S1 *m)
{
  s->flag = 1;
  s->size = m->size;
}

void bar1 (struct S1 *s, struct S1 *m, _Bool flag)
{
  s->flag = flag;
  s->size = m->size;
}

struct S2 {
  unsigned int size : 31;
  unsigned int flag : 1;
};

void foo2 (struct S2 *s, struct S2 *m)
{
  s->size = m->size;
  s->flag = 1;
}

void bar2 (struct S2 *s, struct S2 *m, _Bool flag)
{
  s->flag = flag;
  s->size = m->size;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 4 "store-merging" } } */
