extern void exit (int);
extern void abort (void);

typedef union
{
  struct
  {
    int f1, f2, f3, f4, f5, f6, f7, f8;
    long int f9, f10;
    int f11;
  } f;
  char s[56];
  long int a;
} T;

__attribute__((noinline))
void
test (T *t)
{
  static int i = 11;
  if (t->f.f1 != i++)
    abort ();
  if (t->f.f2 || t->f.f3 || t->f.f4 || t->f.f5 || t->f.f6
      || t->f.f7 || t->f.f8 || t->f.f9 || t->f.f10 || t->f.f11)
    abort ();
  if (i == 20)
    exit (0);
}

__attribute__((noinline))
void
foo (int i)
{
  T t;
again:
  t = (T) { { ++i, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } };
  test (&t);
  goto again;
}

int
main (void)
{
  T *t1, *t2;
  int cnt = 0;
  t1 = (T *) 0;
loop:
  t2 = t1;
  t1 = & (T) { .f.f9 = cnt++ };
  if (cnt < 3)
    goto loop;
  if (t1 != t2 || t1->f.f9 != 2)
    abort ();
  foo (10);
  return 0;
}
