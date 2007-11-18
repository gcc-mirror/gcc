// PR c++/30988
// { dg-do compile }
// { dg-options "-O2 -Wall" }

void f (const char *);

template <typename T> struct A
{
  int g ()
  {
    f (__FUNCTION__);
  }	// { dg-warning "no return statement in function returning non-void" }
};
