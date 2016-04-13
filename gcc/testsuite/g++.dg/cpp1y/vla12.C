// Test to verify that variable length arrays the product of whose constant
// bounds overflows or exceeds the implementation-defined limit are diagnosed.
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-error=vla" }

#define INT_MAX    __INT_MAX__
#define LONG_MAX   __LONG_MAX__
#define SIZE_MAX   __SIZE_MAX__

typedef __SIZE_TYPE__ size_t;

#define MAX (SIZE_MAX / 2)

void test (int x)
{
  const size_t amax = MAX;

  // The following are valid and shouldn't elicit a bounds overflow warning.
  {
    char a [x][amax];         // { dg-warning "forbids" }
    (void)a;
  }

  {
    char a [amax][x];         // { dg-warning "forbids" }
    (void)a;
  }

  // The following is invalid and should be diagnosed.  Unfortunately,
  // when the VLA maximum size is (SIZE_MAX / 2), G++ also issues
  // a (bogus) -Woverflow because it computes the array bound in
  // a signed type (ssize_t) instead of size_t, in addition to
  // rejecting the declaration with error: size of array ‘a’ is too
  // large, before the VLA constant bound check has had a chance to
  // see it.  So the test is disabled.
  // {
  //   char a [x][amax + 1];
  //   (void)a;
  // }

  {
    char a [x][x][amax];      // { dg-warning "forbids" }
    (void)a;
  }

  {
    char a [x][amax][x];      // { dg-warning "forbids" }
    (void)a;
  }

  {
    char a [amax][x][x];      // { dg-warning "forbids" }
    (void)a;
  }

  {
    char a [2][x][amax];      // { dg-warning "forbids|exceeds maximum" }
    (void)a;
  }

  {
    // Unfortunately, the following is rejected with a different error
    // earlier during parsing and before the VLA checking gets to see
    // it: error: size of array ‘a’ is too large
    // Ditto for other multidimensional VLAs where the overflow occurs
    // in the computation of the product of adjacent constant bounds.
    // char a [x][amax][amax];
    // char b [x][2][amax];
    // That error above also leads to the following error when using
    // the variable below.
    //   error:’ was not declared in this scope
    // (void)a;
  }

  {
    char a [amax][x][amax];   // { dg-warning "forbids|exceeds maximum" }
    (void)a;
  }

  {
    char a [amax][amax][x];   // { dg-warning "forbids|exceeds maximum" }
    (void)a;
  }

  {
    struct A256 { __attribute__ ((aligned (256))) char a; };

    enum {
      M = 1024,
      N = MAX / (sizeof (A256) * M)
    };

    A256 a [x][M][x][N];      // { dg-warning "forbids" }
    (void)a;

    A256 b [2][x][M][x][N];   // { dg-warning "forbids|exceeds maximum" }
    (void)b;
  }
}
