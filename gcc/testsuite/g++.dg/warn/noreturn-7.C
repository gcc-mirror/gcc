// PR c++/30988
// { dg-do compile }
// { dg-options "-O2 -Wall" }

void f (const char *) __attribute__ ((noreturn));
void f (int);
void f (double) __attribute__ ((noreturn));

template <typename T> struct A
{
  int g ()
  {
    f ((T) 0);
  }	// { dg-warning "no return statement in function returning non-void" }
};
