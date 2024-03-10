// { dg-additional-options "-O0 -fno-analyzer-suppress-followups -fexceptions" }
#include <new>

struct A
{
  int x;
  int y;
};

void test_spurious_null_warning_throwing ()
{
  int *x = new int; /* { dg-bogus "dereference of possibly-NULL" } */
  int *y = new int (); /* { dg-bogus "dereference of possibly-NULL" "non-throwing" } */
  int *arr = new int[3]; /* { dg-bogus "dereference of possibly-NULL" } */ 
  A *a = new A (); /* { dg-bogus "dereference of possibly-NULL" "throwing new cannot be null" } */

  int z = *y + 2;
  z = *x + 4; /* { dg-bogus "dereference of possibly-NULL 'x'" } */
  /* { dg-warning "use of uninitialized value '\\*x'" "" { target *-*-* } .-1 } */
  z = arr[0] + 4; /* { dg-bogus "dereference of possibly-NULL" } */
  /* { dg-warning "use of uninitialized value '\\*arr'" "" { target *-*-* } .-1 } */

  delete a;
  delete y;
  delete x;
  delete[] arr;
}

void test_default_initialization ()
{
    int *y = ::new int;
    int *x = ::new int (); /* { dg-bogus "dereference of possibly-NULL 'operator new" } */

    int b = *x + 3; /* { dg-bogus "dereference of possibly-NULL" } */
    /* { dg-bogus "use of uninitialized ‘*x’" "" { target *-*-* } .-1 } */
    int a = *y + 2; /* { dg-bogus "dereference of possibly-NULL 'y'" } */
    /* { dg-warning "use of uninitialized value '\\*y'" "no default init" { target *-*-* } .-1 } */

    delete x;
    delete y;
}

/* From clang core.uninitialized.NewArraySize
new[] should not be called with an undefined size argument */

void test_garbage_new_array ()
{
  int n;
  int *arr = ::new int[n]; /* { dg-warning "use of uninitialized value 'n'" } */
  arr[0] = 7;
  ::delete[] arr; /* no warnings emitted here either */
}

void test_nonthrowing ()
{
  int* x = new(std::nothrow) int;
  int* y = new(std::nothrow) int();
  int* arr = new(std::nothrow) int[10];

  int z = *y + 2;  /* { dg-warning "dereference of NULL 'y'" } */
  /* { dg-bogus "use of uninitialized value '\\*y'" "" { target *-*-* } .-1 } */
  z = *x + 4; /* { dg-warning "dereference of possibly-NULL 'x'" } */
  /* { dg-warning "use of uninitialized value '\\*x'" "" { target *-*-* } .-1 } */
  z = arr[0] + 4; /* { dg-warning "dereference of possibly-NULL 'arr'" } */
  /* { dg-warning "use of uninitialized value '\\*arr'" "" { target *-*-* } .-1 } */

  delete y;
  delete x;
  delete[] arr;
}
