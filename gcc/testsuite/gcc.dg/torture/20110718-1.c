/* { dg-do run } */

extern void abort (void);
struct X {
#if (__SIZEOF_LONG__ != __SIZEOF_INT__) && (__SIZEOF_LONG__ == 8)
  unsigned long i : 33;
#else
  unsigned long i;
#endif
};
unsigned long __attribute__((noinline))
foo (struct X *p)
{
  return ~p->i;
}
int main()
{
  struct X x;
  x.i = -1;
  if (foo (&x) != 0)
    abort ();
  return 0;
}
