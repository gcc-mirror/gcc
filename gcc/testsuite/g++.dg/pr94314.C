/* PR c++/94314.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-cddce-details -fdelete-null-pointer-checks" } */

struct A
{
  __attribute__((malloc,noinline))
  static void* operator new(__SIZE_TYPE__ sz)
  {
    ++count;
    return ::operator new(sz);
  }

  static void operator delete(void* ptr)
  {
    --count;
    ::operator delete(ptr);
  }

  static int count;
};

int A::count = 0;

struct B
{
  __attribute__((malloc,noinline))
  static void* operator new(__SIZE_TYPE__ sz)
  {
    ++count;
    return ::operator new(sz);
  }

  __attribute__((noinline))
  static void operator delete(void* ptr)
  {
    --count;
    ::operator delete(ptr);
  }

  static int count;
};

int B::count = 0;

struct C
{
  static void* operator new(__SIZE_TYPE__ sz)
  {
    ++count;
    return ::operator new(sz);
  }

  static void operator delete(void* ptr)
  {
    --count;
    ::operator delete(ptr);
  }

  static int count;
};

int C::count = 0;

int main(){
  delete new A;
  if (A::count != 0)
    __builtin_abort ();

  delete new B;
  if (B::count != 0)
    __builtin_abort ();

  delete new C;
  if (C::count != 0)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Deleting : operator delete" 1 "cddce1"} } */
/* { dg-final { scan-tree-dump-not "Deleting : B::operator delete" "cddce1"} } */
