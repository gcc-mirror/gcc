/* Check that execution counts for template functions
   are reported correctly by gcov. */

#include <stdio.h>
#include <stdlib.h>

/* { dg-options "-fprofile-arcs -ftest-coverage -fno-inline" } */
/* { dg-do run { target native } } */

class A {
  int count;
 public:
  A(int c) { count = c; }
  void func(void) { printf("func\n"); }
  bool done(void) { 
    return (count == 0) ? true : (count-- != 0);
  }
  void run(void) { abort(); }
};

//typedef A T;
template<class T>
void WithoutBrace(T *a) {
  while (!a->done())   
    a->run();           /* count(#####) */
}                       /* count(1) */

template<class T>
void WithBrace(T *a)
{
  while (!a->done())   
    { 
      a->run();         /* count(#####) */
    }
}                       /* count(1) */

A *func(A *a)
{
  WithoutBrace(a);
  WithBrace(a);
  return a;
}

int main() {
  A a(0);
  func(&a);
  return 0;
}

/* { dg-final { run-gcov gcov-5.C } } */
