// Copyright (C) 2002 Free Software Foundation
// Origin: C++/1058
// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>
// { dg-do compile }

struct A {
   typedef int Y;
   typedef double Z;
};

struct B {
   template<typename T>
   void func(typename T::Y, typename T::Z) { }
};

template<typename T>
struct X {
   void gunc();
};

template<typename T>
void X<T>::gunc()
{
   B b;
   b.func<A>(0, 3.);
}

int main()
{
   X<int> x;
   x.gunc();
   return 0;
}
