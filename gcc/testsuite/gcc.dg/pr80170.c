/* { dg-do run } */
/* { dg-options "-fgimple -O2 -ftree-slp-vectorize" } */
/* { dg-require-effective-target ptr32plus } */

struct  A
{
  void * a;
  void * b;
};

struct __attribute__((aligned(16))) B
{
  void * pad;
  void * misaligned;
  void * pad2;

  struct A a;
};

__attribute__((noclone, noinline))
void __GIMPLE (startwith("slp"))
NullB (void * misalignedPtr)
{
  struct B * b;

  bb_2:
  b_2 = misalignedPtr_1(D) + _Literal (__SIZETYPE__) -__SIZEOF_POINTER__;
  __MEM <struct B> (b_2).a.a = _Literal (void *) 0;
  __MEM <struct B> (b_2).a.b = _Literal (void *) 0;
  return;

}

int main()
{
  struct B b;
  NullB (&b.misaligned);
  return 0;
}
