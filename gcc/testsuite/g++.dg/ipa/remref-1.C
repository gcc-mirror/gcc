/* Verify that indirect-inlining induced removal of referenes will not remove
   too many references in presence of speculative devirtualization.  */
/* { dg-do link } */
/* { dg-options "-O3 -fno-early-inlining"  } */

class A
{
  public:
  virtual void foo(void (*)(void));
};

static
void b(void)
{
}

void
A::foo(void (*back)(void))
{
  back();
}

class A *a;

void __attribute__ ((noinline, noclone))
allocate_a ()
{
  a = new A();
}

main()
{
  allocate_a();
  for (int i=0; i<10000;i++)
    a->foo(b);
}
