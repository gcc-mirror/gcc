// Copyright (C) 2002 Free Software Foundation
// Origin: C++/70
// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>
// { dg-do compile }

template <class T>
  struct S;

template <class T>
  void operator* (S<T>, S<T>);

template <class T>
  struct S
  {
     friend void operator*<> (S, S); // { }   // okay
     void operator* (T) { }
  };

template <class T>
  void operator* (S<T>, S<T>) { }  

int main()
{
   S<int> s1, s2;
   s1 * s2;
   s1 * 2;
}
