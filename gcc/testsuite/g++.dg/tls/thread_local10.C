// PR c++/58624

// { dg-do run { target c++11 } }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }

int i;

template <typename> struct A
{
  static thread_local int s;

  A () { i = s; }
};

int f() { return 42; }
template <typename T> thread_local int A<T>::s = f();

int main () {
  A<void> a;
  if (i != 42)
    __builtin_abort();
}
