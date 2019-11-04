// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++2a } }

void f(const int(*)[]);
void fb(const int(*)[3]);
void f2(const int(&)[]);
void fb2(const int(&)[3]);

void
g ()
{
  int arr[3];
  f(&arr);
  fb(&arr);
  f2(arr);
  fb2(arr);
}
