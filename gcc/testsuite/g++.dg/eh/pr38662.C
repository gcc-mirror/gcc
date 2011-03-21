// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ilp32 } } }
class E { };

class T {
  int foo(bool a) throw (E) __attribute__((regparm(1)));
  int bar(bool b) __attribute__((regparm(1)));
};

int T::bar(bool b)
{
  return (b ? 1 : 2);
}

