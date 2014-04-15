/* { dg-do run } */
/* { dg-options "-fschedule-insns" { target scheduling } } */

extern void abort (void);

struct S {
    int i;
    int j;
};

struct U {
    struct S s;
} __attribute__((may_alias));

int __attribute__((noinline,noclone))
foo (struct U *p, struct U *q)
{
  int i;
  q->s.j = 1;
  i = p->s.i;
  return i;
}

int main()
{
  int a[3];
  int *p = a;
  p[1] = 0;
  if (foo ((struct U *)(p + 1), (struct U *)p) != 1)
    abort ();
  return 0;
}
