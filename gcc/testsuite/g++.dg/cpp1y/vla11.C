// PR c++/69517 - [5/6 regression] SEGV on a VLA with excess initializer
//   elements
// PR c++/70019 - VLA size overflow not detected
//
// Runtime test to verify that attempting to either construct a VLA with
// erroneous bounds, or initialize one with an initializer-list that
// contains more elements than the VLA's non-constant (runtime) bounds
// causes an exception to be thrown.  Test also verifies that valid
// VLAs and their initializers don't cause such an exception.

// { dg-do run { target c++11 } }
// { dg-additional-options "-Wno-vla" }

#pragma GCC diagnostic ignored "-Wvla"

#define INT_MAX    __INT_MAX__
#define LONG_MAX   __LONG_MAX__
#define SIZE_MAX   __SIZE_MAX__
#define UINT_MAX   (~0U)
#define ULONG_MAX  (~0LU)

#define INT_MIN    (-__INT_MAX__ - 1)
#define LONG_MIN   (-__LONG_MAX__ - 1)

// The size of the largest allowed VLA in bytes.  Bigger objects
// cause an exception to be thrown.  Unless the maximum size is
// obscenely large, smaller objects should be successfully created
// provided there's enough stack space.  See TEST_NEAR_VLA_MAX_SIZE
// below.
#define MAX   (__SIZE_MAX__ / 2)

// Define to non-zero to exercise very large VLAs with size just
// below the implementation-defined maximum.
#define TEST_NEAR_VLA_MAX_SIZE    0

// Define to zero to enable tests that cause an ICE due to c++/58646.
#define BUG_58646 1

// Helper macro to make it possible to pass as one multpile arguments
// to another macro.
#define Init(...) __VA_ARGS__

typedef __SIZE_TYPE__ size_t;

// Incremented for each test failure.
int fail;

// Used to convert a constant array dimension to a non-constant one.
template <class T>
T d (T n)
{
  return n;
}

// Verify either that an expected exception has been thrown or that
// one hasn't been thrown if one isn't expected.
int __attribute__ ((noclone, noinline))
sink (void *p, int line, bool expect, const char *expr)
{
  if (!p != expect)
    {
      __builtin_printf ("line %i: Assertion failed: '%s': "
                        "exception unexpectedly %sthrown\n",
                        line, expr, !p ? "" : "not ");
      ++fail;
    }
  else
    {
#if defined DEBUG && DEBUG
    __builtin_printf ("line %i: Assertion passed: '%s': "
		      "exception %sthrown as expected\n",
		      line, expr, !p ? "" : "not ");
#endif
    }
  return 0;
}

#define _CAT(name, line) name ## line
#define CAT(name, line) _CAT (name, line)

#define STR(...) #__VA_ARGS__

// Type to exercise VLA with.  TYPESIZE is the size of the type in bytes.
// Using a template serves two purposes.  First, it makes it possible to
// parameterize the test on VLAs of different size.  Second, it verifies
// that the checking code can deal with templates (i.e., completes
// the element type of the VLA when necessary).
template <unsigned TypeSize>
union TestType
{
  char data;
  char padding [TypeSize];
};

// Test function invoked with a pointer to each test case.  Must
// return a value though what value doesn't matter.
int __attribute__ ((noclone, noinline))
tester (int (*testcase)(const char*),
	const char *str, int line, bool expect)
{
  try
    {
      return testcase (str);
    }
  catch (...)
    {
      return sink (0, line, expect, str);
    }
}

// Macro to define a unique specialization of a function template to
// exercise a VLA of type T, rank N, with dimensions given by Dims
// and initializer Init.  Expect is true when the VLA initialization
// is expected to trigger an exception.
// The macro creates a unique global dummy int object and initializes
// it with the result of the function.  The dummy object servers no
// other purpose but to call the function.  The function verifies
// the expected postconditions.
#define TEST(TypeSize, Dims, Init, Expect)				\
  static int CAT (testcase, __LINE__)(const char *str)			\
  {									\
    TestType<TypeSize> vla Dims Init;					\
    static_assert (sizeof (TestType<TypeSize>) == TypeSize,		\
		   "wrong test type size");				\
    return sink (vla, __LINE__, Expect, str);				\
  }									\
  const int CAT (dummy, __LINE__)					\
    = tester (CAT (testcase, __LINE__),					\
	      "T<" #TypeSize "> a" #Dims " " STR (Init) ";",		\
	      __LINE__, Expect)


// Create and run a test function exercising a VLA definition
// of one of the following forms:
//    TestType<Size> VLA Dims;        // uninitialized (with Init ())
// or:
//    TestType<Size> VLA Dims Init;   // initialized (with = Init ({...})
//
//    +-- Element Size (in Bytes)
//    |  +-- VLA Dimensions (constant as in [3], otherwise d(3))
//    |  |         +-- VLA Initializer Expression (if any)
//    |  |         |                  +-- Expect Exception
//    |  |         |                  |
//    V  V         V                  V
TEST (1, [d(0)],   Init (/* none*/),  true);   // uninitialized

#if !BUG_58646
// The following causes an ICE due to c++/58646.
TEST (1, [d(0)],   Init ({}),         true);
#endif
TEST (1, [d(0)],   Init ({1}),        true);   // initialized with " {1}"
TEST (1, [d(0)],   = Init ({1}),      true);   // initialized with "= {1}"

TEST (1, [d(1)],   Init (),           false);
TEST (1, [d(1)],   Init ({}),         false);
TEST (1, [d(1)],   = Init ({}),       false);
TEST (1, [d(1)],   Init ({1}),        false);
TEST (1, [d(1)],   = Init ({1}),      false);
TEST (1, [d(1)],   Init ({1, 2}),     true);
TEST (1, [d(1)],   = Init ({1, 2}),   true);

TEST (1, [d(2)],   Init (),           false);
TEST (1, [d(2)],   Init ({}),         false);
TEST (1, [d(2)],   Init ({1}),        false);
TEST (1, [d(2)],   Init ({1, 2}),     false);
TEST (1, [d(2)],   Init ({1, 2, 3}),  true);

#if TEST_NEAR_VLA_MAX_SIZE
// Very large but not erroneous one dimensional VLAs.
TEST (1, [d(MAX)], Init (),           false);
TEST (1, [d(MAX)], Init ({}),         false);
TEST (1, [d(MAX)], Init ({1}),        false);
TEST (1, [d(MAX)], Init ({1, 2}),     false);
TEST (1, [d(MAX)], Init ({1, 2, 3}),  false);

TEST ( 2, [d(MAX / 2)],   Init (),    false);
TEST ( 4, [d(MAX / 4)],   Init (),    false);
TEST ( 8, [d(MAX / 8)],   Init (),    false);
TEST (16, [d(MAX / 16)],  Init (),    false);
TEST (32, [d(MAX / 32)],  Init (),    false);
TEST (64, [d(MAX / 64)],  Init (),    false);
#endif   // TEST_NEAR_VLA_MAX_SIZE

// One dimensional VLAs with a negative upper bound.
TEST (1, [d(LONG_MIN)],  Init (),       true);
TEST (1, [d(INT_MIN)],   Init (),       true);
TEST (1, [d(-1234)],     Init (),       true);
TEST (1, [d(-1)],        Init (),       true);

// Excessively large one dimensional VLAs.
TEST ( 1, [d(MAX + 1)],   Init (),      true);
TEST ( 2, [d(MAX)],       Init (),      true);
TEST ( 4, [d(MAX / 2)],   Init (),      true);
TEST ( 4, [d(MAX / 3)],   Init (),      true);
TEST ( 8, [d(MAX / 2)],   Init (),      true);
TEST ( 8, [d(MAX / 3)],   Init (),      true);
TEST ( 8, [d(MAX / 4)],   Init (),      true);
TEST ( 8, [d(MAX / 5)],   Init (),      true);
TEST ( 8, [d(MAX / 6)],   Init (),      true);
TEST ( 8, [d(MAX / 7)],   Init (),      true);
TEST (16, [d(MAX / 15)],  Init (),      true);
TEST (32, [d(MAX / 31)],  Init (),      true);
TEST (64, [d(MAX / 63)],  Init (),      true);
TEST ( 1, [d(SIZE_MAX)],  Init (),      true);

TEST (1, [d(LONG_MIN)],  Init ({}),     true);
TEST (1, [d(INT_MIN)],   Init ({}),     true);
TEST (1, [d(-1)],        Init ({}),     true);

TEST (1, [d(SIZE_MAX)],  Init ({}),     true);

TEST (1, [d(LONG_MIN)],  Init ({0}),    true);
TEST (1, [d(INT_MIN)],   Init ({0}),    true);
TEST (1, [d(-1)],        Init ({0}),    true);

TEST (1, [d(SIZE_MAX)],  Init ({0}),    true);

TEST ( 1, [d(SIZE_MAX/2)  + 1], Init (), true);
TEST ( 2, [d(SIZE_MAX/4)  + 1], Init (), true);
TEST ( 4, [d(SIZE_MAX/8)  + 1], Init (), true);
TEST ( 8, [d(SIZE_MAX/16) + 1], Init (), true);
TEST (16, [d(SIZE_MAX/32) + 1], Init (), true);

TEST ( 1, [d(SIZE_MAX/2)  + 1], Init ({1}),             true);
TEST ( 2, [d(SIZE_MAX/4)  + 1], Init ({1, 2}),          true);
TEST ( 4, [d(SIZE_MAX/8)  + 1], Init ({1, 2, 3}),       true);
TEST ( 8, [d(SIZE_MAX/16) + 1], Init ({1, 2, 3, 4}),    true);
TEST (16, [d(SIZE_MAX/32) + 1], Init ({1, 2, 3, 4, 5}), true);

// Two dimensional VLAs with one constant bound.

TEST (1, [1][d(0)],   Init (),          true);

#if !BUG_58646
// The following causes an ICE due to c++/58646.
TEST (1, [1][d(0)],   Init ({}),        true);
#endif
TEST (1, [ ][d(0)],   Init ({{1}}),     true);   // unspecified bound
TEST (1, [1][d(0)],   Init ({{1}}),     true);

TEST (1, [1][d(1)],   Init (),             false);
TEST (1, [1][d(1)],   Init ({{1}}),        false);
TEST (1, [1][d(1)],   Init ({{1, 2}}),     true);
TEST (1, [ ][d(1)],   Init ({{1, 2}}),     true);

TEST (1, [1][d(2)],   Init (),             false);
TEST (1, [1][d(2)],   Init ({{1}}),        false);
TEST (1, [1][d(2)],   Init ({{1, 2}}),     false);
TEST (1, [ ][d(2)],   Init ({{1, 2}}),     false);
TEST (1, [1][d(2)],   Init ({{1, 2, 3}}),  true);
TEST (1, [ ][d(2)],   Init ({{1, 2, 3}}),  true);

TEST (1, [2][d(1)],   Init (),                 false);
TEST (1, [2][d(1)],   Init ({{1}}),            false);
TEST (1, [ ][d(1)],   Init ({{1}}),            false);
TEST (1, [2][d(1)],   Init ({{1}, {2}}),       false);
TEST (1, [ ][d(1)],   Init ({{1}, {2}}),       false);
TEST (1, [2][d(1)],   Init ({{1, 2}}),         true);
TEST (1, [ ][d(1)],   Init ({{1, 2}}),         true);
TEST (1, [2][d(1)],   Init ({{1}, {2, 3}}),    true);
TEST (1, [ ][d(1)],   Init ({{1}, {2, 3}}),    true);
TEST (1, [2][d(1)],   Init ({{1, 2, 3}}),      true);
TEST (1, [ ][d(1)],   Init ({{1, 2, 3}}),      true);
TEST (1, [2][d(1)],   Init ({{1, 2, 3}, {4}}), true);
TEST (1, [ ][d(1)],   Init ({{1, 2, 3}, {4}}), true);
TEST (1, [2][d(1)],   Init ({{1, 2}, {3, 4}}), true);
TEST (1, [ ][d(1)],   Init ({{1, 2}, {3, 4}}), true);

TEST (1, [2][d(2)],   Init (),                       false);
TEST (1, [2][d(2)],   Init ({{1}}),                  false);
TEST (1, [2][d(2)],   Init ({{1, 2}}),               false);
TEST (1, [2][d(2)],   Init ({{1, 2}, {3}}),          false);
TEST (1, [2][d(2)],   Init ({{1, 2}, {3, 4}}),       false);
TEST (1, [2][d(2)],   Init ({{1}, {2, 3, 4}}),       true);
TEST (1, [2][d(2)],   Init ({{1}, {2, 3, 4, 5}}),    true);
TEST (1, [2][d(2)],   Init ({{1, 2}, {3, 4, 5}}),    true);
TEST (1, [2][d(2)],   Init ({{1, 2, 3}, {4, 5}}),    true);
TEST (1, [2][d(2)],   Init ({{1, 2, 3}, {4, 5, 6}}), true);

TEST (1, [2][d(3)],   Init (),                          false);
TEST (1, [2][d(3)],   Init ({{1}}),                     false);
TEST (1, [2][d(3)],   Init ({{1, 2}}),                  false);
TEST (1, [2][d(3)],   Init ({{1, 2}, {3}}),             false);
TEST (1, [2][d(3)],   Init ({{1, 2}, {3, 4}}),          false);
TEST (1, [2][d(3)],   Init ({{1}, {2, 3, 4}}),          false);
TEST (1, [2][d(3)],   Init ({{1}, {2, 3, 4, 5}}),       true);
TEST (1, [2][d(3)],   Init ({{1, 2}, {3, 4, 5}}),       false);
TEST (1, [2][d(3)],   Init ({{1, 2, 3}, {4, 5}}),       false);
TEST (1, [2][d(3)],   Init ({{1, 2, 3}, {4, 5, 6}}),    false);
TEST (1, [2][d(3)],   Init ({{1, 2, 3}, {4, 5, 6, 7}}), true);
TEST (1, [2][d(3)],   Init ({{1, 2, 3, 4}, {5, 6, 7}}), true);
TEST (1, [2][d(3)],   Init ({{1, 2, 3, 4, 5}, {6, 7}}), true);
TEST (1, [2][d(3)],   Init ({{1, 2, 3, 4, 5, 6}, {7}}), true);
TEST (1, [2][d(3)],   Init ({{1, 2, 3, 4, 5, 6, 7}}),   true);

#if TEST_NEAR_VLA_MAX_SIZE
TEST (1, [1][d(MAX)], Init (),                 false);
#  if !BUG_58646
// The following causes an ICE due to c++/58646.
TEST (1, [1][d(MAX)], Init ({}),               false);
#  endif
TEST (1, [1][d(MAX)], Init ({{1}}),            false);
TEST (1, [1][d(MAX)], Init ({{1, 2}}),         false);
TEST (1, [1][d(MAX)], Init ({{1, 2, 3}}),      false);
TEST (1, [1][d(MAX)], Init ({{1, 2, 3, 4}}),   false);

TEST (1, [2][d(MAX / 2)], Init (),                       false);
TEST (1, [2][d(MAX / 2)], Init ({{1}}),                  false);
TEST (1, [2][d(MAX / 2)], Init ({{1, 2}}),               false);
TEST (1, [2][d(MAX / 2)], Init ({{1, 2, 3}}),            false);
TEST (1, [2][d(MAX / 2)], Init ({{1, 2, 3, 4}}),         false);
TEST (1, [2][d(MAX / 2)], Init ({{1}, {2}}),             false);
TEST (1, [2][d(MAX / 2)], Init ({{1}, {2, 3}}),          false);
TEST (1, [2][d(MAX / 2)], Init ({{1, 2}, {3}}),          false);
TEST (1, [2][d(MAX / 2)], Init ({{1, 2}, {3, 4}}),       false);
TEST (1, [2][d(MAX / 2)], Init ({{1, 2, 3}, {4}}),       false);
TEST (1, [2][d(MAX / 2)], Init ({{1, 2, 3}, {4, 5}}),    false);
TEST (1, [2][d(MAX / 2)], Init ({{1, 2, 3}, {4, 5, 6}}), false);
#endif   // TEST_NEAR_VLA_MAX_SIZE

// Excessively large two dimensional VLAs.
TEST (1, [1][d(LONG_MIN)],  Init (),       true);
TEST (1, [1][d(INT_MIN)],   Init (),       true);
TEST (1, [1][d(-1)],        Init (),       true);

TEST (1, [1][d(SIZE_MAX)],  Init (),       true);

#if !BUG_58646
// The following cause an ICE due to c++/58646.
TEST (1, [1][d(LONG_MIN)],  Init ({}),     true);
TEST (1, [1][d(INT_MIN)],   Init ({}),     true);
TEST (1, [1][d(-1)],        Init ({}),     true);
TEST (1, [1][d(SIZE_MAX)],  Init ({}),     true);
#endif

TEST (1, [1][d(LONG_MIN)],  Init ({{0}}),  true);
TEST (1, [1][d(INT_MIN)],   Init ({{0}}),  true);
TEST (1, [1][d(-1)],        Init ({{0}}),  true);
TEST (1, [1][d(SIZE_MAX)],  Init ({{0}}),  true);

TEST (1, [d(LONG_MIN)][1],  Init (),       true);
TEST (1, [d(INT_MIN)][1],   Init (),       true);
TEST (1, [d(-1)][1],        Init (),       true);
TEST (1, [d(SIZE_MAX)][1],  Init (),       true);

TEST (1, [d(LONG_MIN)][1],  Init ({}),     true);
TEST (1, [d(INT_MIN)][1],   Init ({}),     true);
TEST (1, [d(-1)][1],        Init ({}),     true);
TEST (1, [d(SIZE_MAX)][1],  Init ({}),     true);

TEST (1, [d(LONG_MIN)][1],  Init ({{0}}),  true);
TEST (1, [d(INT_MIN)][1],   Init ({{0}}),  true);
TEST (1, [d(-1)][1],        Init ({{0}}),  true);
TEST (1, [d(SIZE_MAX)][1],  Init ({{0}}),  true);

// Two dimensional VLAs with no constant bound.
TEST (1, [d(0)][d(0)],   Init (),          true);
TEST (1, [d(0)][d(0)],   Init ({}),        true);
#if !BUG_58646
// The following cause an ICE due to c++/58646.
TEST (1, [d(0)][d(0)],   Init ({{}}),      true);
TEST (1, [d(0)][d(0)],   Init ({{}, {}}),  true);
#endif

TEST (1, [d(0)][d(0)],   Init ({{1}}),     true);
TEST (1, [d(0)][d(0)],   Init ({{1, 2}}),  true);
#if !BUG_58646
TEST (1, [d(0)][d(0)],   Init ({{1}, {}}), true);
TEST (1, [d(0)][d(0)],   Init ({{}, {1}}), true);
#endif

TEST (1, [d(1)][d(0)],   Init (),          true);
TEST (1, [d(1)][d(0)],   Init ({}),        true);
TEST (1, [d(1)][d(0)],   Init ({{1}}),     true);

TEST (1, [d(1)][d(1)],   Init (),             false);
TEST (1, [d(1)][d(1)],   Init ({{1}}),        false);
TEST (1, [d(1)][d(1)],   Init ({{1, 2}}),     true);

TEST (1, [d(1)][d(2)],   Init (),             false);
TEST (1, [d(1)][d(2)],   Init ({{1}}),        false);
TEST (1, [d(1)][d(2)],   Init ({{1, 2}}),     false);
TEST (1, [d(1)][d(2)],   Init ({{1, 2, 3}}),  true);

TEST (1, [d(2)][d(1)],   Init (),                 false);
TEST (1, [d(2)][d(1)],   Init ({{1}}),            false);
TEST (1, [d(2)][d(1)],   Init ({{1}, {2}}),       false);
TEST (1, [d(2)][d(1)],   Init ({{1, 2}}),         true);
TEST (1, [d(2)][d(1)],   Init ({{1}, {2, 3}}),    true);
TEST (1, [d(2)][d(1)],   Init ({{1, 2, 3}}),      true);
TEST (1, [d(2)][d(1)],   Init ({{1, 2, 3}, {4}}), true);
TEST (1, [d(2)][d(1)],   Init ({{1, 2}, {3, 4}}), true);

TEST (1, [d(2)][d(2)],   Init (),                       false);
TEST (1, [d(2)][d(2)],   Init ({{1}}),                  false);
TEST (1, [d(2)][d(2)],   Init ({{1, 2}}),               false);
TEST (1, [d(2)][d(2)],   Init ({{1, 2}, {3}}),          false);
TEST (1, [d(2)][d(2)],   Init ({{1, 2}, {3, 4}}),       false);
TEST (1, [d(2)][d(2)],   Init ({{1}, {2, 3, 4}}),       true);
TEST (1, [d(2)][d(2)],   Init ({{1}, {2, 3, 4, 5}}),    true);
TEST (1, [d(2)][d(2)],   Init ({{1, 2}, {3, 4, 5}}),    true);
TEST (1, [d(2)][d(2)],   Init ({{1, 2, 3}, {4, 5}}),    true);
TEST (1, [d(2)][d(2)],   Init ({{1, 2, 3}, {4, 5, 6}}), true);

TEST (1, [d(2)][d(3)],   Init (),                          false);
TEST (1, [d(2)][d(3)],   Init ({{1}}),                     false);
TEST (1, [d(2)][d(3)],   Init ({{1, 2}}),                  false);
TEST (1, [d(2)][d(3)],   Init ({{1, 2}, {3}}),             false);
TEST (1, [d(2)][d(3)],   Init ({{1, 2}, {3, 4}}),          false);
TEST (1, [d(2)][d(3)],   Init ({{1}, {2, 3, 4}}),          false);
TEST (1, [d(2)][d(3)],   Init ({{1}, {2, 3, 4, 5}}),       true);
TEST (1, [d(2)][d(3)],   Init ({{1, 2}, {3, 4, 5}}),       false);
TEST (1, [d(2)][d(3)],   Init ({{1, 2, 3}, {4, 5}}),       false);
TEST (1, [d(2)][d(3)],   Init ({{1, 2, 3}, {4, 5, 6}}),    false);
TEST (1, [d(2)][d(3)],   Init ({{1, 2, 3}, {4, 5, 6, 7}}), true);
TEST (1, [d(2)][d(3)],   Init ({{1, 2, 3, 4}, {5, 6, 7}}), true);
TEST (1, [d(2)][d(3)],   Init ({{1, 2, 3, 4, 5}, {6, 7}}), true);
TEST (1, [d(2)][d(3)],   Init ({{1, 2, 3, 4, 5, 6}, {7}}), true);
TEST (1, [d(2)][d(3)],   Init ({{1, 2, 3, 4, 5, 6, 7}}),   true);

#if TEST_NEAR_VLA_MAX_SIZE
TEST (1, [d(1)][d(MAX)], Init (),                              false);
TEST (1, [d(1)][d(MAX)], Init ({}),                            false);
TEST (1, [d(1)][d(MAX)], Init ({{1}}),                         false);
TEST (1, [d(1)][d(MAX)], Init ({{1, 2}}),                      false);
TEST (1, [d(1)][d(MAX)], Init ({{1, 2, 3}}),                   false);
TEST (1, [d(1)][d(MAX)], Init ({{1, 2, 3, 4}}),                false);
TEST (1, [d(1)][d(MAX)], Init ({{1, 2, 3, 4, 5}}),             false);
TEST (1, [d(1)][d(MAX)], Init ({{1, 2, 3, 4, 5, 6}}),          false);
TEST (1, [d(1)][d(MAX)], Init ({{1, 2, 3, 4, 5, 6, 7}}),       false);
TEST (1, [d(1)][d(MAX)], Init ({{1, 2, 3, 4, 5, 6, 7, 8}}),    false);
TEST (1, [d(1)][d(MAX)], Init ({{1, 2, 3, 4, 5, 6, 7, 8, 9}}), false);

TEST (1, [d(2)][d(MAX / 2)], Init (),                              false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1}}),                         false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2}}),                      false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2, 3}}),                   false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2, 3, 4}}),                false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2, 3, 4, 5}}),             false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2, 3, 4, 5, 6}}),          false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2, 3, 4, 5, 6, 7}}),       false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2, 3, 4, 5, 6, 7, 8}}),    false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2, 3, 4, 5, 6, 7, 8, 9}}), false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1}, {2}}),                    false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1}, {2, 3}}),                 false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2}, {3}}),                 false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2}, {3, 4}}),              false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2, 3}, {4}}),              false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2, 3}, {4, 5}}),           false);
TEST (1, [d(2)][d(MAX / 2)], Init ({{1, 2, 3}, {4, 5, 6}}),        false);
#endif

TEST (1, [d(2)][d(MAX)],        Init (),                         true);
TEST (1, [d(2)][d(MAX)],        Init ({{1}}),                    true);
TEST (1, [d(MAX)][d(MAX)],      Init ({{1, 2}}),                 true);
TEST (1, [d(0)][d(MAX)],        Init ({{1}, {2}}),               true);
TEST (1, [d(INT_MAX)][d(MAX)],  Init ({{1}, {2, 3}}),            true);
TEST (1, [d(SIZE_MAX)][d(MAX)], Init ({{1, 2}, {3, 4}, {5}}),    true);

// Erroneous two-dimensional VLAs with size exceeding SIZE_MAX / 2
// (those must be rejected because no object can be bigger than that,
// otherwise pointer arithmetic breaks).
TEST ( 1, [2][d(SIZE_MAX/2)],  Init (), true);
TEST ( 2, [2][d(SIZE_MAX/4)],  Init (), true);
TEST ( 4, [2][d(SIZE_MAX/8)],  Init (), true);
TEST ( 8, [2][d(SIZE_MAX/16)], Init (), true);
TEST (16, [2][d(SIZE_MAX/32)], Init (), true);

TEST ( 1, [d(SIZE_MAX/2)][2],  Init (), true);
TEST ( 2, [d(SIZE_MAX/4)][2],  Init (), true);
TEST ( 4, [d(SIZE_MAX/8)][2],  Init (), true);
TEST ( 8, [d(SIZE_MAX/16)][2], Init (), true);
TEST (16, [d(SIZE_MAX/32)][2], Init (), true);

// Verify that the unspecified bound is factored into the computation
// of the total size.
TEST ( 1, [][d(SIZE_MAX/2)],  Init ({{1}, {2}}),      true);
TEST ( 2, [][d(SIZE_MAX/4)],  Init ({{1}, {2}}),      true);
TEST ( 4, [][d(SIZE_MAX/8)],  Init ({{1}, {2}}),      true);
TEST ( 8, [][d(SIZE_MAX/16)], Init ({{1}, {2}}),      true);
TEST (16, [][d(SIZE_MAX/32)], Init ({{1}, {2}}),      true);
TEST (16, [][d(SIZE_MAX/64)], Init ({{1}, {2}, {3}}), true);

// Three dimensional VLAs with two constant bounds.

TEST (1, [1][1][d(-1)], Init (),                    true);
TEST (1, [1][1][d(0)], Init (),                     true);

#if !BUG_58646
// The following causes an ICE due to c++/58646.
TEST (1, [1][1][d(0)],  Init ({}),                   true);
TEST (1, [1][1][d(-1)], Init ({{}}),                 true);
TEST (1, [1][d(-1)][1], Init ({{}}),                 true);
TEST (1, [d(-1)][1][1], Init ({{}}),                 true);

TEST (1, [1][1][d(0)], Init ({{}}),                  true);
TEST (1, [1][d(0)][1], Init ({{}}),                  true);
TEST (1, [d(0)][1][1], Init ({{}}),                  true);
#endif

TEST (1, [1][1][d(1)], Init (),                      false);

#if !BUG_58646
TEST (1, [1][1][d(1)], Init ({{}}),                  false);
TEST (1, [1][1][d(1)], Init ({{{}}}),                false);
TEST (1, [1][1][d(1)], Init ({{{1}}}),               false);
#endif

TEST (1, [1][1][d(1)], Init ({{{1, 2}}}),            true);
TEST (1, [1][1][d(1)], Init ({{{1, 2, 3}}}),         true);

TEST (1, [1][d(1)][1], Init (),                      false);

#if !BUG_58646
TEST (1, [1][d(1)][1], Init ({{}}),                  false);
TEST (1, [1][d(1)][1], Init ({{{}}}),                false);
#endif

TEST (1, [1][d(1)][1], Init ({{{1}}}),               false);
TEST (1, [1][d(1)][1], Init ({{{1}, {2}}}),          true);
TEST (1, [1][d(1)][1], Init ({{{1}, {2}, {3}}}),     true);

TEST (1, [d(1)][1][1], Init (),                      false);

#if !BUG_58646
TEST (1, [d(1)][1][1], Init ({{}}),                  false);
TEST (1, [d(1)][1][1], Init ({{{}}}),                false);
#endif

TEST (1, [d(1)][1][1], Init ({{{1}}}),               false);
TEST (1, [d(1)][1][1], Init ({{{1}}, {{2}}}),        true);
TEST (1, [d(1)][1][1], Init ({{{1}}, {{2}}, {{3}}}), true);

TEST (1, [1][1][d(2)], Init (),                      false);

#if !BUG_58646
TEST (1, [1][1][d(2)], Init ({{}}),                  false);
TEST (1, [1][1][d(2)], Init ({{{}}}),                false);
#endif

TEST (1, [1][1][d(2)], Init ({{{1}}}),               false);
TEST (1, [1][1][d(2)], Init ({{{1, 2}}}),            false);
TEST (1, [1][1][d(2)], Init ({{{1, 2, 3}}}),         true);

TEST (1, [1][d(2)][1], Init (),                      false);

#if !BUG_58646
TEST (1, [1][d(2)][1], Init ({{}}),                  false);
TEST (1, [1][d(2)][1], Init ({{{}}}),                false);
#endif
TEST (1, [1][d(2)][1], Init ({{{1}}}),               false);
TEST (1, [1][d(2)][1], Init ({{{1}, {2}}}),          false);
TEST (1, [1][d(2)][1], Init ({{{1}, {2}, {3}}}),     true);

TEST (1, [d(2)][1][1], Init (),                      false);

#if !BUG_58646
TEST (1, [d(2)][1][1], Init ({{}}),                  false);
TEST (1, [d(2)][1][1], Init ({{{}}}),                false);
#endif
TEST (1, [d(2)][1][1], Init ({{{1}}}),               false);
TEST (1, [d(2)][1][1], Init ({{{1}}, {{2}}}),        false);
TEST (1, [d(2)][1][1], Init ({{{1}}, {{2}}, {{3}}}), true);

TEST (1, [1][2][d(2)], Init (),                      false);

#if !BUG_58646
TEST (1, [1][2][d(2)], Init ({{}}),                  false);
TEST (1, [1][2][d(2)], Init ({{{}}}),                false);
#endif

TEST (1, [1][2][d(2)], Init ({{{1}}}),               false);
TEST (1, [1][2][d(2)], Init ({{{1, 2}}}),            false);
TEST (1, [1][2][d(2)], Init ({{{1, 2, 3}}}),         true);

TEST (1, [1][2][d(2)], Init ({{{1}, {2}}}),          false);
TEST (1, [1][2][d(2)], Init ({{{1}, {2, 3}}}),       false);
TEST (1, [1][2][d(2)], Init ({{{1, 2}, {3}}}),       false);
TEST (1, [1][2][d(2)], Init ({{{1, 2}, {3, 4}}}),    false);
TEST (1, [1][2][d(2)], Init ({{{1}, {2, 3, 4}}}),    true);
TEST (1, [1][2][d(2)], Init ({{{1, 2, 3}, {}}}),     true);
TEST (1, [1][2][d(2)], Init ({{{1, 2, 3}, {4}}}),    true);
TEST (1, [1][2][d(2)], Init ({{{1, 2, 3, 4}}}),      true);
TEST (1, [1][2][d(2)], Init ({{{1, 2, 3, 4}, {}}}),  true);
TEST (1, [1][2][d(2)], Init ({{{1, 2, 3, 4}, {5}}}), true);

TEST (1, [2][2][d(2)], Init ({{{1}, {2}}}),                         false);
TEST (1, [2][2][d(2)], Init ({{{1}, {2, 3}}}),                      false);
TEST (1, [2][2][d(2)], Init ({{{1, 2}}}),                           false);
TEST (1, [2][2][d(2)], Init ({{{1, 2}, {3}}}),                      false);
TEST (1, [2][2][d(2)], Init ({{{1, 2}, {3, 4}}}),                   false);
TEST (1, [2][2][d(2)], Init ({{{1, 2}, {3, 4}}, {{5}}}),            false);
TEST (1, [2][2][d(2)], Init ({{{1, 2}, {3, 4}}, {{5, 6}}}),         false);
TEST (1, [2][2][d(2)], Init ({{{1, 2}, {3, 4}}, {{5, 6}, {7}}}),    false);
TEST (1, [2][2][d(2)], Init ({{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}), false);

TEST (1, [2][2][d(2)], Init ({{{1}, {2, 3, 4}}}),                      true);
TEST (1, [2][2][d(2)], Init ({{{1, 2, 3}, {}}}),                       true);
TEST (1, [2][2][d(2)], Init ({{{1, 2, 3}, {4}}}),                      true);
TEST (1, [2][2][d(2)], Init ({{{1, 2, 3, 4}}}),                        true);
TEST (1, [2][2][d(2)], Init ({{{1, 2, 3, 4}, {}}}),                    true);
TEST (1, [2][2][d(2)], Init ({{{1, 2, 3, 4}, {5}}}),                   true);
TEST (1, [2][2][d(2)], Init ({{{1, 2}, {3, 4}}, {{5, 6}, {7, 8, 9}}}), true);
TEST (1, [2][2][d(2)], Init ({{{1, 2}, {3, 4}}, {{5, 6, 7}, {8, 9}}}), true);
TEST (1, [2][2][d(2)], Init ({{{1, 2}, {3, 4, 5}}, {{6, 7}, {8, 9}}}), true);
TEST (1, [2][2][d(2)], Init ({{{1, 2, 3}, {4, 5}}, {{6, 7}, {8, 9}}}), true);
TEST (1, [2][2][d(2)], Init ({{{1}, {2}}, {{3}, {4, 5, 6}}}),          true);
TEST (1, [2][2][d(2)], Init ({{{1}}, {{2}, {3, 4, 5, 6}}}),            true);

// Three dimensional VLAs with one constant bound.
TEST (1, [2][d(-1)][d(-1)], Init (),                                      true);
TEST (1, [2][d(-1)][d(0)],  Init (),                                      true);
TEST (1, [2][d(0)][d(-1)],  Init (),                                      true);
TEST (1, [2][d(1)][d(-1)],  Init (),                                      true);
TEST (1, [2][d(1)][d(0)],   Init (),                                      true);
TEST (1, [2][d(-1)][d(1)],  Init (),                                      true);
TEST (1, [2][d(0)][d(1)],   Init (),                                      true);

TEST (1, [2][d(2)][d(2)], Init (),                                        false);
TEST (1, [2][d(2)][d(2)], Init ({{{1}}}),                                 false);
TEST (1, [ ][d(2)][d(2)], Init ({{{1}}}),                                 false);
TEST (1, [2][d(2)][d(2)], Init ({{{1}, {2}}}),                            false);
TEST (1, [ ][d(2)][d(2)], Init ({{{1}, {2}}}),                            false);
TEST (1, [2][d(2)][d(2)], Init ({{{1}, {2, 3}}}),                         false);
TEST (1, [ ][d(2)][d(2)], Init ({{{1}, {2, 3}}}),                         false);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2}, {3}}}),                         false);
TEST (1, [ ][d(2)][d(2)], Init ({{{1, 2}, {3}}}),                         false);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2}, {3, 4}}}),                      false);
TEST (1, [ ][d(2)][d(2)], Init ({{{1, 2}, {3, 4}}}),                      false);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}),    false);
TEST (1, [ ][d(2)][d(2)], Init ({{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}),    false);
TEST (1, [2][d(2)][d(2)], Init ({{{1}, {2, 3, 4}}}),                      true);
TEST (1, [ ][d(2)][d(2)], Init ({{{1}, {2, 3, 4}}}),                      true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3}, {}}}),                       true);
TEST (1, [ ][d(2)][d(2)], Init ({{{1, 2, 3}, {}}}),                       true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3}, {4}}}),                      true);
TEST (1, [ ][d(2)][d(2)], Init ({{{1, 2, 3}, {4}}}),                      true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3, 4}}}),                        true);
TEST (1, [ ][d(2)][d(2)], Init ({{{1, 2, 3, 4}}}),                        true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3, 4}, {}}}),                    true);
TEST (1, [ ][d(2)][d(2)], Init ({{{1, 2, 3, 4}, {}}}),                    true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3, 4}, {5}}}),                   true);
TEST (1, [ ][d(2)][d(2)], Init ({{{1, 2, 3, 4}, {5}}}),                   true);
TEST (1, [2][d(2)][d(2)], Init ({{{1}, {2, 3, 4}}}),                      true);
TEST (1, [ ][d(2)][d(2)], Init ({{{1}, {2, 3, 4}}}),                      true);
TEST (1, [2][d(2)][d(2)], Init ({{{1}, {2, 3}, {4}}}),                    true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3}, {}}}),                       true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3}, {4}}}),                      true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3, 4}}}),                        true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3, 4}, {}}}),                    true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3, 4}, {5}}}),                   true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2}, {3, 4}}, {{5, 6}, {7, 8, 9}}}), true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2}, {3, 4}}, {{5, 6, 7}, {8, 9}}}), true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2}, {3, 4, 5}}, {{6, 7}, {8, 9}}}), true);
TEST (1, [2][d(2)][d(2)], Init ({{{1, 2, 3}, {4, 5}}, {{6, 7}, {8, 9}}}), true);
TEST (1, [2][d(2)][d(2)], Init ({{{1}, {2}}, {{3}, {4, 5, 6}}}),          true);
TEST (1, [2][d(2)][d(2)], Init ({{{1}}, {{2}, {3, 4, 5, 6}}}),            true);
TEST (1, [2][d(2)][d(2)], Init ({{{1}, {2}, {3}}}),                       true);
TEST (1, [2][d(2)][d(2)], Init ({{{1}, {2, 3}, {4}}}),                    true);
TEST (1, [2][d(2)][d(2)], Init ({{{1}, {2, 3, 4}, {5}}}),                 true);

#if TEST_NEAR_VLA_MAX_SIZE
// Very large but not erroneous three-dimensional VLAs.
TEST ( 1, [2][d(1)][d(MAX/2)], Init (),  false);
TEST ( 2, [2][d(1)][d(MAX/4)], Init (),  false);
TEST ( 4, [2][d(1)][d(MAX/8)], Init (),  false);
TEST ( 8, [2][d(1)][d(MAX/16)], Init (), false);
TEST (16, [2][d(1)][d(MAX/32)], Init (), false);

TEST ( 1, [2][d(MAX/2)][d(1)], Init (),  false);
TEST ( 2, [2][d(MAX/4)][d(1)], Init (),  false);
TEST ( 4, [2][d(MAX/8)][d(1)], Init (),  false);
TEST ( 8, [2][d(MAX/16)][d(1)], Init (), false);
TEST (16, [2][d(MAX/32)][d(1)], Init (), false);

TEST ( 1, [d(MAX/2)][2][d(1)], Init (),  false);
TEST ( 2, [d(MAX/4)][2][d(1)], Init (),  false);
TEST ( 4, [d(MAX/8)][2][d(1)], Init (),  false);
TEST ( 8, [d(MAX/16)][2][d(1)], Init (), false);
TEST (16, [d(MAX/32)][2][d(1)], Init (), false);
#endif   // TEST_NEAR_VLA_MAX_SIZE

// Erroneous three-dimensional VLAs with size exceeding SIZE_MAX / 2
// (those must be rejected because no object can be bigger than that,
// otherwise pointer arithmetic breaks).
TEST ( 1, [2][d(1)][d(SIZE_MAX/2)],  Init (), true);
TEST ( 2, [2][d(1)][d(SIZE_MAX/4)],  Init (), true);
TEST ( 4, [2][d(1)][d(SIZE_MAX/8)],  Init (), true);
TEST ( 8, [2][d(1)][d(SIZE_MAX/16)], Init (), true);
TEST (16, [2][d(1)][d(SIZE_MAX/32)], Init (), true);

TEST ( 1, [2][d(SIZE_MAX/2)][d(1)],  Init (), true);
TEST ( 2, [2][d(SIZE_MAX/4)][d(1)],  Init (), true);
TEST ( 4, [2][d(SIZE_MAX/8)][d(1)],  Init (), true);
TEST ( 8, [2][d(SIZE_MAX/16)][d(1)], Init (), true);
TEST (16, [2][d(SIZE_MAX/32)][d(1)], Init (), true);

TEST ( 1, [d(SIZE_MAX/2)][2][d(1)],  Init (), true);
TEST ( 2, [d(SIZE_MAX/4)][2][d(1)],  Init (), true);
TEST ( 4, [d(SIZE_MAX/8)][2][d(1)],  Init (), true);
TEST ( 8, [d(SIZE_MAX/16)][2][d(1)], Init (), true);
TEST (16, [d(SIZE_MAX/32)][2][d(1)], Init (), true);

TEST (16, [3][d(SIZE_MAX)][d(SIZE_MAX)], Init (), true);
TEST (32, [d(SIZE_MAX)][5][d(SIZE_MAX)], Init (), true);
TEST (64, [d(SIZE_MAX)][d(SIZE_MAX)][7], Init (), true);

int main ()
{
  if (fail)
    __builtin_abort ();
}
