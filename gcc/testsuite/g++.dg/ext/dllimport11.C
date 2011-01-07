// PR target/23589
// Template member functions do not get dllimport status of class.
// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw* x86_64-*-mingw* } }

struct __attribute__((dllimport)) Foo
{
  template <class T> Foo (T);
};

void a (int i)
{
  Foo f(i);
}

template <class T>  Foo::Foo (T) {}   // no dllimport warnings on definition.
