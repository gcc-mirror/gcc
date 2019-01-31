// { dg-do compile }
// { dg-options "-Wmissing-braces" }

struct S { };
typedef void (S::*fptr1) (int);

struct A {
  fptr1 f;
};

A a[] =
{
 (fptr1) 0,
}; // { dg-warning "missing braces around initializer" }

A a2[] =
{
 { (fptr1) 0 }
};
