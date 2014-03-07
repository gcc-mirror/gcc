// PR c++/58672
// { dg-do compile { target c++11 } }
// { dg-require-effective-target tls }

struct A
{
  A(int);
  i;				// { dg-error "" }
};

thread_local A a(0);
