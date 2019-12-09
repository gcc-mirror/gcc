// C++ PR/2521
// Copyright (C) 2002 Free Software Foundation
// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

// { dg-do compile }

struct A 
{
  void f();
  void foo(void (A::*)(int));       // { dg-message "void A::foo|no known conversion" }
  template<typename T>
    void g(T);
  void h()
  {
    void (A::*p)() = &A::f;
    void (A::*q)() = &(A::f);       // { dg-error "27:ISO C\\+\\+ forbids taking the address of an unqualified or parenthesized" }
    foo(&g<int>);                   // { dg-error "cannot convert" }
  }
};
