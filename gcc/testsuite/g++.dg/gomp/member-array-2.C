#include <cassert>

typedef int intarr100[100];

class C {
  int arr[100];
  int *ptr;

public:
  C();
  ~C();
  void zero ();
  void do_operation ();
  void check (int, int);
  intarr100 &get_arr () { return arr; }
  int *get_ptr() { return ptr; }
};

C::C()
{
  ptr = new int[100];
  for (int i = 0; i < 100; i++)
    arr[i] = 0;
}

C::~C()
{
  delete ptr;
}

void
C::zero ()
{
  for (int i = 0; i < 100; i++)
    arr[i] = ptr[i] = 0;
}

void
C::do_operation ()
{
#pragma omp target map(arr, ptr, ptr[:100])
#pragma omp teams distribute parallel for
  for (int i = 0; i < 100; i++)
    {
      arr[i] = arr[i] + 3;
      ptr[i] = ptr[i] + 5;
    }
}

void
C::check (int arrval, int ptrval)
{
  for (int i = 0; i < 100; i++)
    {
      assert (arr[i] == arrval);
      assert (ptr[i] == ptrval);
    }
}

int
main (int argc, char *argv[])
{
  C c;

  c.zero ();
  c.do_operation ();
  c.check (3, 5);

  /* It might sort of make sense to be able to do this, but we don't support
     it for now.  */
  #pragma omp target map(c.get_arr()[:100])
  /* { dg-message {sorry, unimplemented: unsupported map expression 'c\.C::get_arr\(\)\[0\]'} "" { target *-*-* } .-1 } */
  #pragma omp teams distribute parallel for
    for (int i = 0; i < 100; i++)
      c.get_arr()[i] += 2;

  c.check (5, 5);

  /* Same for this.  */
  #pragma omp target map(c.get_ptr(), c.get_ptr()[:100])
  /* { dg-message {sorry, unimplemented: unsupported map expression 'c\.C::get_ptr\(\)'} "" { target *-*-* } .-1 } */
  /* { dg-message {sorry, unimplemented: unsupported map expression '\* c\.C::get_ptr\(\)'} "" { target *-*-* } .-2 } */
  #pragma omp teams distribute parallel for
    for (int i = 0; i < 100; i++)
      c.get_ptr()[i] += 3;

  c.check (5, 8);

  return 0;
}

