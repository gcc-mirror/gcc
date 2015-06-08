/* { dg-do run } */
/* { dg-options "-O2 -fipa-ra" } */

extern void abort (void);

#if (__SIZEOF_LONG_LONG__ == __SIZEOF_POINTER__)
#define ADD_SUFFIX(a) a ## ULL
#elif (__SIZEOF_LONG__ == __SIZEOF_POINTER__)
#define ADD_SUFFIX(a) a ## UL
#elif (__SIZEOF_INT__ == __SIZEOF_POINTER__)
#define ADD_SUFFIX(a) a ## U
#else
#error Add target support here
#endif

#if __SIZEOF_POINTER__ <= 4
/* Use a 16 bit pointer to have a valid pointer for 16-bit to 31-bit pointer
   architectures.  Using sizeof, we cannot distinguish between 31-bit and 32-bit
   pointer types, so we also handle the 32-bit pointer type case here.  */
#define CONST_PTR ADD_SUFFIX (0x800)
#else
/* For x86_64 -m64, the problem reproduces with this 32-bit CONST_PTR, but not
   with a 2-power below it.  */
#define CONST_PTR ADD_SUFFIX (0x80000000)
#endif

int __attribute__((noinline, noclone))
bar (void)
{
  return 1;
}

struct S
{
  unsigned long p, q, r;
  void *v;
};

struct S *s1;
struct S *s2;

void __attribute__((noinline, noclone))
fn2 (struct S *x)
{
  s2 = x;
}

__attribute__((noinline, noclone)) void *
fn1 (struct S *x)
{
  /* Just a statement to make it a non-const function.  */
  s1 = x;

  return (void *)0;
}

int __attribute__((noinline, noclone))
baz (void)
{
  struct S *x = (struct S *) CONST_PTR;

  x += bar ();

  fn1 (x);
  fn2 (x);

  return 0;
}

int
main (void)
{
  baz ();

  if (s2 != (((struct S *) CONST_PTR) + 1))
    abort ();

  return 0;
}
