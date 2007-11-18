// PR c++/30988
// { dg-do compile }
// { dg-options "-O2 -Wall" }

void f (const char *) __attribute__ ((noreturn));

template <typename T> struct A
{
  int g ()
  {
    f (__FUNCTION__);
  }
};
