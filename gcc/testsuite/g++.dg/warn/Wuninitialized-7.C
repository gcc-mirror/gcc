// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O -Wuninitialized" }

struct A {
    A (int);
};

struct B: A {
    const bool x = true;

    B (): A (x ? 3 : 7) { } // { dg-warning "x. is used uninitialized" }
};

void f (void*);
void g ()
{
  B b;
  f (&b);
}
