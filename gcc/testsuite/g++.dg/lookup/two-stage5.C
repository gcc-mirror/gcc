// { dg-do run }
extern "C" void abort ();

namespace N {
template <class T> T foo (T) { return T (); }
template <class T> T bar (T t) { return foo (t); }
}

struct S { S (int i = 0): i_ (i) { } int i_; };

namespace N {
/* template <> */ S foo (S) { return S (1); }
}

int main ()
{
  if (1 == N::bar (S()).i_)
     abort ();
} 
