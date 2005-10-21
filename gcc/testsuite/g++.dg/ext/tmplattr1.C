// PR c++/24260
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target ilp32 }

#define stdcall __attribute__((stdcall))

struct T {
  template <class S>
  static int stdcall func(int arg1, int arg2);
};

template <class S>
int stdcall T::func(int arg1, int arg2)
{
  return arg1+arg2;
}

struct dummy {};

void xx()
{
  int (stdcall *ptr2)(int,int) = &T::func<dummy>;
}

