// Test for C99-style designated array initializer

union U
{
  long l;
  const char *p;
};

__extension__ U u = { .p = "" };

__extension__ int i[4] = { [0] = 1, [1] = 2 };

// Currently, except for unions, the C++ front end only supports
// designators that designate the element that would have been initialized
// anyway, except that C++2A designators can skip over some direct
// non-static data members.  While that's true, make sure that we get
// a sorry rather than bad code.

struct A
{
  int i;
  int j;
};

__extension__ A a = { .j = 1 };
__extension__ A b = { .j = 2, .i = 1 }; // { dg-error "designator order for field 'A::i' does not match declaration order in 'A'" }
__extension__ int j[2] = { [1] = 1 }; // { dg-message "non-trivial" }
