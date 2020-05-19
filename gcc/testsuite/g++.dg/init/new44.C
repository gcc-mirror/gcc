// { dg-do compile }
// { dg-options "-ftrack-macro-expansion=0" }

// Test for PR c++/67927 - array new expression with excessive number
// of elements not diagnosed.

// GCC uses a different maximum value at compile time and at runtime:
// 1) The compile-time maximum, MAX, is SIZE_MAX / 2 minus the size
//    of a cookie (sizeof (size_t)).  Exceeding the compile-time
//    maximum is ill-formed and diagnosed.  This test verifies this
//    diagnostic.
// 2) The runtime runtime maximum is the most significant 7 bits,
//    starting with the first most significant non-zero bit, of
//    the dividend of the compile-time constant MAX and the product
//    of the constant array dimensions and the element size, minus
//    the size of the "cookie."  This is also roughly (though not
//    exactly) SIZE_MAX / 2.  Exceeding the runtime maximum is
//    diagnosed at runtime by throwing a bad_array_new_length
//    exception.
//    The cookie is the number of elements in the array, and is
//    only added for non-POD types, but the its size factors into
//    the maximum size formula regardless.

// See also PR c++/19351 - integer overflow in operator new[].

// For convenience.
#define MAX __SIZE_MAX__

typedef __typeof__ (sizeof 0) size_t;

void* operator new (size_t, void*);
void* operator new[] (size_t, void*);

void *p;

// Exercise new expression with one-dimensional arrays of char.
static void __attribute__ ((used))
test_one_dim_char_array ()
{
    p = new char [MAX];                 // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX - 1];             // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX - 2];             // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX - 99];            // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2];             // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2 - 1];         // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }
    p = new char [MAX / 2 - 2];         // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid testing the expressions below since whether or not they
    // are accepted depends on the precision of size_t (which also
    // determines the size of the cookie).
    // p = new char [MAX / 2 - 3];
    // p = new char [MAX / 2 - 4];
    // p = new char [MAX / 2 - 5];
    // p = new char [MAX / 2 - 6];

    // The following expressions are accepted on ILP32 as well LP64
    // (they will be diagnosed on LP128 if there ever is such a data
    // model).
    p = new char [MAX / 2 - 7];         // okay
    p = new char [MAX / 2 - 8];         // okay
}

static void __attribute__ ((used))
test_one_dim_short_array ()
{
    p = new short [MAX];                // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX - 1];            // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX - 2];            // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX - 99];           // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX / 2];            // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX / 2 - 1];        // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX / 2 - 2];        // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX / 2 - 3];        // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX / 2 - 4];        // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX / 2 - 5];        // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX / 2 - 6];        // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX / 2 - 7];        // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX / 2 - 8];        // { dg-error "size .\[0-9\]+. of array" }
    p = new short [MAX / 4];            // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new short [MAX / 4 - 1];

    p = new short [MAX / 4 - 4];        // okay
}

// Exercise new expression with two-dimensional arrays or char.
static void __attribute__ ((used))
test_two_dim_char_array ()
{
    p = new char [1][MAX];              // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][MAX - 1];          // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][MAX - 2];          // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][MAX - 99];         // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][MAX / 2];          // { dg-error "size .\[0-9\]+. of array" }
    p = new char [1][MAX / 2 - 1];      // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }
    p = new char [1][MAX / 2 - 2];      // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new char [1][MAX / 2 - 3];
    // p = new char [1][MAX / 2 - 4];
    // p = new char [1][MAX / 2 - 5];
    // p = new char [1][MAX / 2 - 6];

    p = new char [1][MAX / 2 - 7];      // okay
    p = new char [1][MAX / 2 - 8];      // okay

    p = new char [2][MAX];              // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX - 1];          // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX - 2];          // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 2];          // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][MAX / 2 - 1];      // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][MAX / 2 - 2];      // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][MAX / 2 - 7];      // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][MAX / 2 - 8];      // { dg-error "size .\[0-9\]+. of array" }

    p = new char [MAX][MAX];            // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [MAX][MAX - 1];        // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [MAX][MAX - 2];        // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [MAX][MAX / 2];        // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX][MAX / 2 - 1];    // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX][MAX / 2 - 2];    // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX][MAX / 2 - 7];    // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX][MAX / 2 - 8];    // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX][2];              // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX][1];              // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2][1];          // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2 - 1][1];      // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }
    p = new char [MAX / 2 - 2][1];      // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new char [MAX / 2 - 3][1];
    // p = new char [MAX / 2 - 4][1];
    // p = new char [MAX / 2 - 5][1];
    // p = new char [MAX / 2 - 6][1];

    p = new char [MAX / 2 - 7][1];      // okay
    p = new char [MAX / 2 - 8][1];      // okay
}


// Exercise new expression with three-dimensional arrays.
static __attribute__ ((used)) void
test_three_dim_char_array ()
{
    p = new char [1][1][MAX];           // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][1][MAX - 1];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][1][MAX - 2];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][1][MAX - 99];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][1][MAX / 2];       // { dg-error "size .\[0-9\]+. of array" }
    p = new char [1][1][MAX / 2 - 1];   // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }
    p = new char [1][1][MAX / 2 - 2];   // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new char [1][1][MAX / 2 - 3];
    // p = new char [1][1][MAX / 2 - 4];
    // p = new char [1][1][MAX / 2 - 5];
    // p = new char [1][1][MAX / 2 - 6];

    p = new char [1][1][MAX / 2 - 7];   // okay
    p = new char [1][1][MAX / 2 - 8];   // okay

    p = new char [1][2][MAX];           // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX - 1];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX - 2];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX - 99];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX / 2];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX / 2 - 1];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX / 2 - 2];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX / 2 - 3];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX / 2 - 4];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX / 2 - 5];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX / 2 - 6];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX / 2 - 7];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX / 2 - 8];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [1][2][MAX / 4];       // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new char [1][2][MAX / 4 - 1];
    // p = new char [1][2][MAX / 4 - 2];

    p = new char [1][2][MAX / 4 - 3];   // okay
    p = new char [1][2][MAX / 4 - 4];   // okay

    p = new char [2][1][MAX];           // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][1][MAX - 1];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][1][MAX - 2];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][1][MAX - 99];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][1][MAX / 2];       // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][1][MAX / 2 - 1];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][1][MAX / 2 - 2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][1][MAX / 2 - 3];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][1][MAX / 2 - 4];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][1][MAX / 2 - 5];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][1][MAX / 2 - 6];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][1][MAX / 2 - 7];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][1][MAX / 2 - 8];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][1][MAX / 4];       // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new char [2][1][MAX / 4 - 1];
    // p = new char [2][1][MAX / 4 - 2];

    p = new char [2][1][MAX / 4 - 3];   // okay
    p = new char [2][1][MAX / 4 - 4];   // okay

    p = new char [2][2][MAX];           // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX - 1];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX - 2];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX - 99];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX / 2];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX / 2 - 1];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX / 2 - 2];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX / 2 - 3];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX / 2 - 4];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX / 2 - 5];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX / 2 - 6];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX / 2 - 7];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX / 2 - 8];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][2][MAX / 4];       // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][2][MAX / 4 - 1];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][2][MAX / 4 - 2];   // { dg-error "size .\[0-9\]+. of array" }

    // Avoid exercising data model-dependent expressions.
    // p = new char [2][2][MAX / 8];
    // p = new char [2][2][MAX / 8 - 1];

    p = new char [2][2][MAX / 8 - 2];
    p = new char [2][2][MAX / 8 - 3];

    p = new char [2][MAX][2];           // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX - 1][2];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX - 2][2];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX - 99][2];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 2][2];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 2 - 1][2];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 2 - 2][2];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 2 - 3][2];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 2 - 4][2];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 2 - 5][2];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 2 - 6][2];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 2 - 7][2];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 2 - 8][2];   // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [2][MAX / 4][2];       // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][MAX / 4 - 1][2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [2][MAX / 4 - 2][2];   // { dg-error "size .\[0-9\]+. of array" }

    // Avoid exercising data model-dependent expressions.
    // p = new char [2][MAX / 8][2];
    // p = new char [2][MAX / 8 - 1][2];

    p = new char [2][MAX / 8 - 2][2];
    p = new char [2][MAX / 8 - 3][2];

    p = new char [MAX][2][2];           // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX - 1][2][2];       // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX - 2][2][2];       // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX - 99][2][2];      // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2][2][2];       // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2 - 1][2][2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2 - 2][2][2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2 - 3][2][2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2 - 4][2][2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2 - 5][2][2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2 - 6][2][2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2 - 7][2][2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 2 - 8][2][2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 4][2][2];       // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 4 - 1][2][2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new char [MAX / 4 - 2][2][2];   // { dg-error "size .\[0-9\]+. of array" }

    // Avoid exercising data model-dependent expressions.
    // p = new char [MAX / 8][2][2];
    // p = new char [MAX / 8 - 1][2][2];

    p = new char [MAX / 8 - 2][2][2];
    p = new char [MAX / 8 - 3][2][2];

    p = new char [MAX][MAX][MAX];         // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [MAX][MAX][MAX / 2];     // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [MAX][MAX / 2][MAX];     // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new char [MAX][MAX / 2][MAX / 2]; // { dg-error "size of (unnamed )?array" }
    p = new char [MAX / 2][MAX / 2][MAX / 2]; // { dg-error "size of (unnamed )?array" }
}

// Exercise new expression with N-dimensional arrays where N is
// sizeof(size_t).
static __attribute__ ((used)) void
test_N_dim_char_array ()
{
#if __SIZEOF_SIZE_T__ == 8
    enum { N = 256 };
#elif __SIZEOF_SIZE_T__ == 4
    enum { N = 16 };
#else
    enum { N = 4 };
#endif

#ifndef __MSP430X_LARGE__  /* 20-bit size_t.  */
    p = new char        [N][N][N][N][N][N][N];
    p = new char [N / 2][2][N][N][N][N][N][N];
    p = new char [N - 1][N / 2][N][N][N][N][N][N];
    p = new char [N / 2][N][N][N][N][N][N][N];  // { dg-error "size .\[0-9\]+. of array" "" { target { ! msp430_large } } }
    p = new char [N - 1][N][N][N][N][N][N][N];  // { dg-error "size .\[0-9\]+. of array" "" { target { ! msp430_large } } }
    p = new char [N]    [N][N][N][N][N][N][N];  // { dg-error "size .\[0-9\]+. of array" "" { target { ! msp430_large } } }
#endif
}

typedef struct Byte {
    char c;

    void* operator new (size_t, void*);
    void* operator new[] (size_t, void*);
} B;

void* operator new (size_t, B*);
void* operator new[] (size_t, B*);

// Exercise placement new expression with one-dimensional arrays of a struct.
static void __attribute__ ((used))
test_one_dim_byte_array (void *p)
{
    p = new (p) B [MAX];                // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX - 1];            // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX - 2];            // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX - 99];           // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2];            // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2 - 1];        // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }
    p = new (p) B [MAX / 2 - 2];        // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid testing the expressions below since whether or not they
    // are accepted depends on the precision of size_t (which determines
    // the size .\[0-9\]+. of the cookie).
    // p = new (p) B [MAX / 2 - 3];
    // p = new (p) B [MAX / 2 - 4];
    // p = new (p) B [MAX / 2 - 5];
    // p = new (p) B [MAX / 2 - 6];

    // The following expressions are accepted on ILP32 as well LP64
    // (they will be diagnosed on LP128 if there ever is such a data
    // model).
    p = new (p) B [MAX / 2 - 7];         // okay
    p = new (p) B [MAX / 2 - 8];         // okay
}

// Exercise placement new expression with two-dimensional arrays.
static void __attribute__ ((used))
test_placement_two_dim_byte_struct_array (void *p)
{
    p = new (p) B [1][MAX];             // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][MAX - 1];         // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][MAX - 2];         // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][MAX - 99];        // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][MAX / 2];         // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [1][MAX / 2 - 1];     // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }
    p = new (p) B [1][MAX / 2 - 2];     // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new (p) B [1][MAX / 2 - 3];
    // p = new (p) B [1][MAX / 2 - 4];
    // p = new (p) B [1][MAX / 2 - 5];
    // p = new (p) B [1][MAX / 2 - 6];

    p = new (p) B [1][MAX / 2 - 7];      // okay
    p = new (p) B [1][MAX / 2 - 8];      // okay

    p = new (p) B [2][MAX];             // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX - 1];         // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX - 2];         // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 2];         // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][MAX / 2 - 1];     // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][MAX / 2 - 2];     // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][MAX / 2 - 7];     // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][MAX / 2 - 8];     // { dg-error "size .\[0-9\]+. of array" }

    p = new (p) B [MAX][MAX];           // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [MAX][MAX - 1];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [MAX][MAX - 2];       // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [MAX][MAX / 2];       // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX][MAX / 2 - 1];   // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX][MAX / 2 - 2];   // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX][MAX / 2 - 7];   // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX][MAX / 2 - 8];   // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX][2];             // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX][1];             // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2][1];         // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2 - 1][1];     // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }
    p = new (p) B [MAX / 2 - 2][1];     // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new (p) B [MAX / 2 - 3][1];
    // p = new (p) B [MAX / 2 - 4][1];
    // p = new (p) B [MAX / 2 - 5][1];
    // p = new (p) B [MAX / 2 - 6][1];

    p = new (p) B [MAX / 2 - 7][1];      // okay
    p = new (p) B [MAX / 2 - 8][1];      // okay
}


// Exercise placement new expression with three-dimensional arrays.
static __attribute__ ((used)) void
test_placement_three_dim_byte_struct_array (void *p)
{
    p = new (p) B [1][1][MAX];          // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][1][MAX - 1];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][1][MAX - 2];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][1][MAX - 99];     // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][1][MAX / 2];      // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [1][1][MAX / 2 - 1];  // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }
    p = new (p) B [1][1][MAX / 2 - 2];  // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new (p) B [1][1][MAX / 2 - 3];
    // p = new (p) B [1][1][MAX / 2 - 4];
    // p = new (p) B [1][1][MAX / 2 - 5];
    // p = new (p) B [1][1][MAX / 2 - 6];

    p = new (p) B [1][1][MAX / 2 - 7];   // okay
    p = new (p) B [1][1][MAX / 2 - 8];   // okay

    p = new (p) B [1][2][MAX];          // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX - 1];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX - 2];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX - 99];     // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX / 2];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX / 2 - 1];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX / 2 - 2];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX / 2 - 3];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX / 2 - 4];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX / 2 - 5];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX / 2 - 6];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX / 2 - 7];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX / 2 - 8];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [1][2][MAX / 4];      // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new (p) B [1][2][MAX / 4 - 1];
    // p = new (p) B [1][2][MAX / 4 - 2];

    p = new (p) B [1][2][MAX / 4 - 3];   // okay
    p = new (p) B [1][2][MAX / 4 - 4];   // okay

    p = new (p) B [2][1][MAX];          // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][1][MAX - 1];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][1][MAX - 2];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][1][MAX - 99];     // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][1][MAX / 2];      // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][1][MAX / 2 - 1];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][1][MAX / 2 - 2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][1][MAX / 2 - 3];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][1][MAX / 2 - 4];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][1][MAX / 2 - 5];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][1][MAX / 2 - 6];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][1][MAX / 2 - 7];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][1][MAX / 2 - 8];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][1][MAX / 4];      // { dg-error "size .\[0-9\]+. of array" "cookie required" { target { ! msp430_small } } }

    // Avoid exercising data model-dependent expressions.
    // p = new (p) B [2][1][MAX / 4 - 1];
    // p = new (p) B [2][1][MAX / 4 - 2];

    p = new (p) B [2][1][MAX / 4 - 3];   // okay
    p = new (p) B [2][1][MAX / 4 - 4];   // okay

    p = new (p) B [2][2][MAX];          // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX - 1];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX - 2];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX - 99];     // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX / 2];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX / 2 - 1];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX / 2 - 2];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX / 2 - 3];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX / 2 - 4];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX / 2 - 5];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX / 2 - 6];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX / 2 - 7];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX / 2 - 8];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][2][MAX / 4];      // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][2][MAX / 4 - 1];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][2][MAX / 4 - 2];  // { dg-error "size .\[0-9\]+. of array" }

    // Avoid exercising data model-dependent expressions.
    // p = new (p) B [2][2][MAX / 8];
    // p = new (p) B [2][2][MAX / 8 - 1];

    p = new (p) B [2][2][MAX / 8 - 2];
    p = new (p) B [2][2][MAX / 8 - 3];

    p = new (p) B [2][MAX][2];          // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX - 1][2];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX - 2][2];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX - 99][2];     // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 2][2];      // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 2 - 1][2];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 2 - 2][2];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 2 - 3][2];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 2 - 4][2];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 2 - 5][2];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 2 - 6][2];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 2 - 7][2];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 2 - 8][2];  // { dg-error "size .\[0-9\]+. of (unnamed )?array" }
    p = new (p) B [2][MAX / 4][2];      // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][MAX / 4 - 1][2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [2][MAX / 4 - 2][2];  // { dg-error "size .\[0-9\]+. of array" }

    // Avoid exercising data model-dependent expressions.
    // p = new (p) B [2][MAX / 8][2];
    // p = new (p) B [2][MAX / 8 - 1][2];

    p = new (p) B [2][MAX / 8 - 2][2];
    p = new (p) B [2][MAX / 8 - 3][2];

    p = new (p) B [MAX][2][2];          // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX - 1][2][2];      // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX - 2][2][2];      // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX - 99][2][2];     // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2][2][2];      // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2 - 1][2][2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2 - 2][2][2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2 - 3][2][2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2 - 4][2][2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2 - 5][2][2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2 - 6][2][2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2 - 7][2][2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 2 - 8][2][2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 4][2][2];      // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 4 - 1][2][2];  // { dg-error "size .\[0-9\]+. of array" }
    p = new (p) B [MAX / 4 - 2][2][2];  // { dg-error "size .\[0-9\]+. of array" }

    // Avoid exercising data model-dependent expressions.
    // p = new (p) B [MAX / 8][2][2];
    // p = new (p) B [MAX / 8 - 1][2][2];

    p = new (p) B [MAX / 8 - 2][2][2];
    p = new (p) B [MAX / 8 - 3][2][2];
}
