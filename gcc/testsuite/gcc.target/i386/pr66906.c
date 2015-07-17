/* { dg-do run { target ia32 } } */
/* { dg-options "-O0 -mregparm=3" } */

typedef int ptrdiff_t;
extern void abort (void);
int
check_int (int *i, int align)
{
  *i = 20;
  if ((((ptrdiff_t) i) & (align - 1)) != 0)
    abort ();
  return *i;
}
void
check (void *p, int align)
{
  if ((((ptrdiff_t) p) & (align - 1)) != 0)
    abort ();
}
typedef int aligned __attribute__((aligned(64)));
void
foo (void)
{
  aligned j;
  void bar ()
    {
      aligned i;
      if (check_int (&i, __alignof__(i)) != i)
	abort ();
      if (check_int (&j, __alignof__(j)) != j)
	abort ();
      j = -20;
    }
  bar ();
  if (j != -20)
    abort ();
  if (check_int (&j, __alignof__(j)) != j)
    abort ();
}
int
main()
{
  foo ();
  return 0;
}
