/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */

/* Test that targets correctly round the size of the outgoing arguments
   to a multiple of STACK_BOUNDARY.  There is a serious alignment bug if
   aligned alloca does not get aligned!  */

__extension__ typedef __UINTPTR_TYPE__ uintptr_t;
extern void abort (void);

volatile int xx;
volatile int x = 16;

void
t1 (int x0, int x1, int x2, int x3, int x4, int x5, int x6, int x7,
    void *p, int align)
{
  xx = x0 + x1 + x2 + x3 + x4 + x4 + x6 + x7;
  if ((int)(uintptr_t)p & (align-1))
    abort ();
}

void
t2 (int x0, int x1, int x2, int x3, int x4, int x5, int x6, int x7,
    void *p, int align, int dummy)
{
  xx = x0 + x1 + x2 + x3 + x4 + x4 + x6 + x7;
  if ((int)(uintptr_t)p & (align-1))
    abort ();
}

void
t1_a4 (int size)
{
  void *p = __builtin_alloca_with_align (size, 32);
  t1 (0, 0, 0, 0, 0, 0, 0, 0, p, 4);
}

void
t2_a4 (int size)
{
  void *p = __builtin_alloca_with_align (size, 32);
  t2 (0, 0, 0, 0, 0, 0, 0, 0, p, 4, 0);
}

void
t1_a8 (int size)
{
  void *p = __builtin_alloca_with_align (size, 64);
  t1 (0, 0, 0, 0, 0, 0, 0, 0, p, 8);
}

void
t2_a8 (int size)
{
  void *p = __builtin_alloca_with_align (size, 64);
  t2 (0, 0, 0, 0, 0, 0, 0, 0, p, 8, 0);
}

void
t1_a16 (int size)
{
  void *p = __builtin_alloca_with_align (size, 128);
  t1 (0, 0, 0, 0, 0, 0, 0, 0, p, 16);
}

void
t2_a16 (int size)
{
  void *p = __builtin_alloca_with_align (size, 128);
  t2 (0, 0, 0, 0, 0, 0, 0, 0, p, 16, 0);
}

void
t1_a32 (int size)
{
  void *p = __builtin_alloca_with_align (size, 256);
  t1 (0, 0, 0, 0, 0, 0, 0, 0, p, 32);
}

void
t2_a32 (int size)
{
  void *p = __builtin_alloca_with_align (size, 256);
  t2 (0, 0, 0, 0, 0, 0, 0, 0, p, 32, 0);
}


int
main ()
{
  t1_a4 (x);
  t2_a4 (x);
  t1_a8 (x);
  t2_a8 (x);
  t1_a16 (x);
  t2_a16 (x);
  t1_a32 (x);
  t2_a32 (x);
  return 0;
}
