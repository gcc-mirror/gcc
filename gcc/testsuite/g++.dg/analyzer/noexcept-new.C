/* { dg-additional-options "-O0 -fno-exceptions -fno-analyzer-suppress-followups" } */
#include <new>

/* Test non-throwing variants of operator new */

struct A
{
  int x;
  int y;
};

void test_throwing ()
{
  int* x = new int;
  int* y = new int(); /* { dg-warning "dereference of possibly-NULL" } */
  int* arr = new int[10];
  A *a = new A(); /* { dg-warning "dereference of possibly-NULL" } */

  int z = *y + 2;
  z = *x + 4; /* { dg-warning "dereference of possibly-NULL 'x'" } */
  /* { dg-warning "use of uninitialized value '\\*x'" "" { target *-*-* } .-1 } */
  z = arr[0] + 4; /* { dg-warning "dereference of possibly-NULL 'arr'" } */
  /* { dg-warning "use of uninitialized value '\\*arr'" "" { target *-*-* } .-1 } */
  a->y = a->x + 3;

  delete a;
  delete y;
  delete x;
  delete[] arr;
}

void test_nonthrowing ()
{
  int* x = new(std::nothrow) int;
  int* y = new(std::nothrow) int();
  int* arr = new(std::nothrow) int[10];

  int z = *y + 2; /* { dg-warning "dereference of NULL 'y'" } */
  /* { dg-bogus "use of uninitialized value '\\*y'" "" { target *-*-* } .-1 } */
  z = *x + 4; /* { dg-warning "dereference of possibly-NULL 'x'" } */
  /* { dg-warning "use of uninitialized value '\\*x'" "" { target *-*-* } .-1 } */
  z = arr[0] + 4; /* { dg-warning "dereference of possibly-NULL 'arr'" } */
  /* { dg-warning "use of uninitialized value '\\*arr'" "" { target *-*-* } .-1 } */

  delete y;
  delete x;
  delete[] arr;
}
