/* { dg-do compile } */

// Test for PR c++/67913 - new expression with negative size not diagnosed.
typedef __typeof__ (sizeof 0) size_t;

void* operator new (size_t, void*);
void* operator new[] (size_t, void*);

struct A {
    int a [4];
};

struct B {
    int a [4];

    void* operator new (size_t, void*);
    void* operator new[] (size_t, void*);
};

void* operator new (size_t, B*);
void* operator new[] (size_t, B*);

void *p;

void test_literal ()
{
    char c;
    (void)c;

    B b;

    // Verify integer literal.
    p = new char [-1];           // { dg-error "size of array is negative" }
    p = new char [2][-3];        // { dg-error "size of array is negative" }
    p = new char [-4][5];        // { dg-error "size of array is negative" }
    p = new char [-6][-7];       // { dg-error "size of array is negative" }

    p = new (p) char [-1];       // { dg-error "size of array is negative" }
    p = new (p) char [2][-3];    // { dg-error "size of array is negative" }
    p = new (p) char [-4][5];    // { dg-error "size of array is negative" }
    p = new (p) char [-6][-7];   // { dg-error "size of array is negative" }

    p = new (p) A [-1];          // { dg-error "size of array is negative" }
    p = new (p) A [2][-3];       // { dg-error "size of array is negative" }
    p = new (p) A [-4][5];       // { dg-error "size of array is negative" }
    p = new (p) A [-6][-7];      // { dg-error "size of array is negative" }

    p = new (p) B [-1];          // { dg-error "size of array is negative" }
    p = new (p) B [2][-3];       // { dg-error "size of array is negative" }
    p = new (p) B [-4][5];       // { dg-error "size of array is negative" }
    p = new (p) B [-6][-7];      // { dg-error "size of array is negative" }

    p = new (&b) B [-1];          // { dg-error "size of array is negative" }
    p = new (&b) B [2][-3];       // { dg-error "size of array is negative" }
    p = new (&b) B [-4][5];       // { dg-error "size of array is negative" }
    p = new (&b) B [-6][-7];      // { dg-error "size of array is negative" }

    p = new char [1 - 2];         // { dg-error "size of array is negative" }
    p = new (p) char [2 - 3];     // { dg-error "size of array is negative" }
    p = new A [2 < 1 ? -1 : -2];  // { dg-error "size of array is negative" }
    p = new (p) B [2 - 3 * 2];    // { dg-error "size of array is negative" }
    p = new (&b) B [1][2 - 3 * 2];// { dg-error "size of array is negative" }
}

void test_constant_expression ()
{
    char c;
    (void)c;

    B b;

    static const signed char i1 = -1;
    static const signed short i2 = -2;
    static const signed int i3 = -3;
    static const signed long i4 = -4;
    static const signed long long i5 = -5;
    static const int i6 = -6;
    static const int i7 = -7;

    // Verify constant expression.
    p = new char [i1];           // { dg-error "size of array is negative" }
    p = new char [2][i3];        // { dg-error "size of array is negative" }
    p = new char [i4][5];        // { dg-error "size of array is negative" }
    p = new char [i6][i7];       // { dg-error "size of array is negative" }

    p = new (p) char [i1];       // { dg-error "size of array is negative" }
    p = new (p) char [2][i3];    // { dg-error "size of array is negative" }
    p = new (p) char [i4][5];    // { dg-error "size of array is negative" }
    p = new (p) char [i6][i7];   // { dg-error "size of array is negative" }

    p = new (p) A [i1];          // { dg-error "size of array is negative" }
    p = new (p) A [2][i3];       // { dg-error "size of array is negative" }
    p = new (p) A [i4][5];       // { dg-error "size of array is negative" }
    p = new (p) A [i6][i7];      // { dg-error "size of array is negative" }

    p = new (p) B [i1];          // { dg-error "size of array is negative" }
    p = new (p) B [2][i3];       // { dg-error "size of array is negative" }
    p = new (p) B [i4][5];       // { dg-error "size of array is negative" }
    p = new (p) B [i6][i7];      // { dg-error "size of array is negative" }

    p = new (&b) B [i1];          // { dg-error "size of array is negative" }
    p = new (&b) B [2][i3];       // { dg-error "size of array is negative" }
    p = new (&b) B [i4][5];       // { dg-error "size of array is negative" }
    p = new (&b) B [i6][i7];      // { dg-error "size of array is negative" }

    p = new short [i1 - 2];       // { dg-error "size of array is negative" }
    p = new (p) bool [i2 - 3];    // { dg-error "size of array is negative" }
    p = new A [2 < 1 ? i1 : i2];  // { dg-error "size of array is negative" }
    p = new (p) B [2 + i3 * 2];   // { dg-error "size of array is negative" }
    p = new (&b) B [1][i1 - 3 * 2];// { dg-error "size of array is negative" }
}

void test_constexpr ()
{
    B b;

#if __cplusplus >= 201103L

    // Verify that a constant expression that is "a prvalue core constant
    // expression whose value is an object where, for that object and its
    // subobjects each non-static data member of reference type refers to
    // an object with static storage duration."
    static constexpr struct S {
        int i_;
        constexpr S (int i): i_ (i) { }
        constexpr operator int () const { return i_; }
    } s1 (-1), s2 (-2), s3 (-3), s4 (-4), s5 (-5), s6 (-6), s7 (-7);
#else
    // C++ 11 constexpr is not available, fall back on plain ole enum.
    enum { s1 = -1, s2 = -2, s3 = -3, s4 = -4, s5 = -5, s6 = -6, s7 = -7 };
#endif

    // Verify constant expression.
    p = new char [s1];           // { dg-error "size of array is negative" }
    p = new char [2][s3];        // { dg-error "size of array is negative" }
    p = new char [s4][5];        // { dg-error "size of array is negative" }
    p = new char [s6][s7];       // { dg-error "size of array is negative" }

    p = new (p) char [s1];       // { dg-error "size of array is negative" }
    p = new (p) char [2][s3];    // { dg-error "size of array is negative" }
    p = new (p) char [s4][5];    // { dg-error "size of array is negative" }
    p = new (p) char [s6][s7];   // { dg-error "size of array is negative" }

    p = new (p) A [s1];          // { dg-error "size of array is negative" }
    p = new (p) A [2][s3];       // { dg-error "size of array is negative" }
    p = new (p) A [s4][5];       // { dg-error "size of array is negative" }
    p = new (p) A [s6][s7];      // { dg-error "size of array is negative" }

    p = new (p) B [s1];          // { dg-error "size of array is negative" }
    p = new (p) B [2][s3];       // { dg-error "size of array is negative" }
    p = new (p) B [s4][5];       // { dg-error "size of array is negative" }
    p = new (p) B [s6][s7];      // { dg-error "size of array is negative" }

    p = new (&b) B [s1];          // { dg-error "size of array is negative" }
    p = new (&b) B [2][s3];       // { dg-error "size of array is negative" }
    p = new (&b) B [s4][5];       // { dg-error "size of array is negative" }
    p = new (&b) B [s6][s7];      // { dg-error "size of array is negative" }

    p = new int [s1 + s2];           // { dg-error "size of array is negative" }
    p = new (p) long [2 * s3];       // { dg-error "size of array is negative" }
    p = new A [s2 < s1 ? s1 : s2];   // { dg-error "size of array is negative" }
    p = new (p) B [s7 - s2 * 2];     // { dg-error "size of array is negative" }
    p = new (&b) B [9][s4 - s1 * 2]; // { dg-error "size of array is negative" }
}
