/* Verify that we survive creation and deletion of references to facilitate
   reference removal while also doing (unsuccessful) speculative
   devirtualization.  */
/* { dg-do link } */
/* { dg-options "-O3 -fno-early-inlining"  } */

class A
{
  public:
  virtual void __attribute__ ((noinline)) foo(void (*)(void));
};

static
void b(void)
{
}

void  __attribute__ ((noinline))
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
