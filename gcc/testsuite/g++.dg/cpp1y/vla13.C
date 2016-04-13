// PR c++/70019 - VLA size overflow not detected
// Runtime test to verify that attempting to initialize a VLA with a string
// or character array that's longer than the non-constant (runtime) bound
// of the VLA causes an exception to be thrown.  For a compile-time version
// of the test see vla14.C.

// { dg-do run { target c++11 } }
// { dg-additional-options "-Wno-vla" }

#pragma GCC diagnostic ignored "-Wvla"

#define SIZE_MAX   __SIZE_MAX__

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

// Define to zero to enable tests that cause an ICE due to c++/69487.
#define BUG_69487 1

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

template <class T, int>
int test ();

#define _CAT(name, line) name ## line
#define CAT(name, line) _CAT (name, line)

#define STR(...) #__VA_ARGS__

// Macro to define a unique specialization of a function template to
// exercise a VLA of type T, rank N, with dimensions given by Dims
// and initializer Init.  Expect is true when the VLA initialization
// is expected to trigger an exception.
// The macro creates a unique global dummy int object and initializes
// it with the result of the function.  The dummy object servers no
// other purpose but to call the function.  The function verifies
// the expected postconditions.
#define TEST(T, Dims, Init, Expect)                                     \
  template <>                                                           \
  int test<T, __LINE__>()                                               \
  {                                                                     \
    const char str[] = "char a" #Dims " = { " STR (Init) " }";          \
    try {                                                               \
      T a Dims = { Init };                                              \
      return sink (a, __LINE__, Expect, str);                           \
    }                                                                   \
    catch (...) {                                                       \
      return sink (0, __LINE__, Expect, str);                           \
    }                                                                   \
  }                                                                     \
  const int CAT (dummy, __LINE__) = test<T, __LINE__>()


// Create and run a test function exercising a VLA definition
//    +-- Element Type
//    |     +-- VLA Dimensions
//    |     |        +-- VLA Initializer
//    |     |        |
//    |     |        |             +-- Expect Exception
//    |     |        |             |
//    V     V        V             V
TEST (char, [d(-1)], "",           true);

TEST (char, [d(0)],  "",           true);
TEST (char, [d(0)],  (""),         true);

TEST (char, [d(1)],  "",           false);
TEST (char, [d(1)],  (""),         false);

TEST (char, [d(1)],  "1",          true);
TEST (char, [d(1)],  ("1"),        true);

TEST (char, [d(1)],  "12",         true);
TEST (char, [d(1)],  "1234567890", true);

TEST (char, [d(2)], "",           false);
TEST (char, [d(2)], (""),         false);

TEST (char, [d(2)], "1",          false);
TEST (char, [d(2)], "12",         true);
TEST (char, [d(2)], "123",        true);
TEST (char, [d(2)], "1234567890", true);

TEST (char, [d(3)], "",           false);
TEST (char, [d(3)], "1",          false);
TEST (char, [d(3)], "12",         false);
TEST (char, [d(3)], "123",        true);
TEST (char, [d(3)], "1234",       true);
TEST (char, [d(3)], "1234567890", true);

#if TEST_NEAR_VLA_MAX_SIZE

#  if !BUG_69487
// The following crash due to c++/69487.
TEST (char, [d(MAX)], "",           false);
TEST (char, [d(MAX)], "1",          false);
TEST (char, [d(MAX)], "12",         false);
TEST (char, [d(MAX)], "1234567890", false);
#  endif

TEST (char, [d(MAX)], Init (),                             false);
TEST (char, [d(MAX)], Init (1),                            false);
TEST (char, [d(MAX)], Init (1, 2),                         false);
TEST (char, [d(MAX)], Init (1, 2, 3, 4, 5, 6, 7, 8, 9, 0), false);
#endif

TEST (char, [d(SIZE_MAX / 2 + 1)], "", true);
TEST (char, [d(SIZE_MAX - 2)],     "", true);
TEST (char, [d(SIZE_MAX - 1)],     "", true);

TEST (wchar_t, [d(1)], L"",           false);
TEST (wchar_t, [d(1)], (L""),         false);
TEST (wchar_t, [d(1)], L"1",          true);
TEST (wchar_t, [d(1)], L"12",         true);
TEST (wchar_t, [d(1)], L"1234567890", true);

TEST (wchar_t, [d(2)], L"",           false);
TEST (wchar_t, [d(2)], L"1",          false);
TEST (wchar_t, [d(2)], L"12",         true);
TEST (wchar_t, [d(2)], L"123",        true);
TEST (wchar_t, [d(2)], L"1234567890", true);

TEST (char, [d(1)][d(1)], Init (""),        false);
TEST (char, [1]   [d(1)], Init (""),        false);
TEST (char, [d(1)][1],    Init (""),        false);

TEST (char, [d(1)][d(1)], Init ("1"),       true);

// The following is accepted at compile time but throws an exception
// at runtime since in C++ a one-element array cannot be initialized
// with a string literal of length one because there isn't room for
// the terminating NUL
TEST (char, [1][d(1)],    Init ("1"),       true);

// The following is rejected at compile-time since a one-element array
// cannot be initialized with a string literal of length one because
// there isn't room for the terminating NUL (see vla14.C).
// TEST (char, [d(1)][1],    Init ("1"),       false);

TEST (char, [d(1)][d(1)], Init ("12"),      true);
TEST (char, [d(1)][d(1)], Init ("1", "2"),  true);
TEST (char, [d(1)][d(1)], Init ("1", "23"), true);

TEST (char, [d(2)][d(2)], Init ("", ""),    false);
TEST (char, [d(2)][d(2)], Init ("", "1"),   false);
TEST (char, [d(2)][d(2)], Init ("1", ""),   false);
TEST (char, [d(2)][d(2)], Init ("1", "1"),  false);
TEST (char, [2][d(2)],    Init ("",  "1"),  false);
TEST (char, [2][d(2)],    Init ("1", ""),   false);
TEST (char, [2][d(2)],    Init ("1", "1"),  false);
TEST (char, [d(2)][2],    Init ("",  "1"),  false);
TEST (char, [d(2)][2],    Init ("1", ""),   false);
TEST (char, [d(2)][2],    Init ("1", "1"),  false);

TEST (char, [2][d(2)],    Init ("1", "23"), true);
TEST (char, [d(2)][d(2)], Init ("1", "23"), true);
TEST (char, [d(2)][d(2)], Init ("1", "23"), true);
TEST (char, [d(2)][d(2)], Init ("12","3"),  true);

#if TEST_NEAR_VLA_MAX_SIZE
#  if !BUG_69487
   // The following crash due to c++/69487.
TEST (char, [1][d(MAX)], Init (""),           false);
TEST (char, [1][d(MAX)], Init ("1"),          false);
TEST (char, [1][d(MAX)], Init ("12"),         false);
TEST (char, [1][d(MAX)], Init ("1234567890"), false);
#  endif

#  if !BUG_58646
// The following causes an ICE due to c++/58646.
TEST (char, [1][d(MAX)], Init (),                               false);
#  endif

TEST (char, [1][d(MAX)], Init ({1}),                            false);
TEST (char, [1][d(MAX)], Init ({1, 2}),                         false);
TEST (char, [1][d(MAX)], Init ({1, 2, 3}),                      false);
TEST (char, [1][d(MAX)], Init ({1, 2, 3, 4, 5, 6, 7, 8, 9, 0}), false);

TEST (char, [d(MAX)][1], Init ({1}),                            false);
TEST (char, [d(MAX)][1], Init ({1}, {2}),                       false);
TEST (char, [d(MAX)][1], Init ({1}, {2}, {3}),                  false);
TEST (char, [d(MAX)][1], Init ({1}, {2}, {3}, {4}, {5},
			       {6}, {7}, {8}, {9}, {0}),        false);
#endif   // TEST_NEAR_VLA_MAX_SIZE

// The following are expected to throw due to excessive size.
TEST (char, [2][d(MAX)], Init ({1}),                                 true);
TEST (char, [2][d(MAX)], Init ({1, 2}),                              true);
TEST (char, [2][d(MAX)], Init ({1}, {2}),                            true);
TEST (char, [2][d(MAX)], Init ({1, 2}, {3, 4}),                      true);
TEST (char, [2][d(MAX)], Init ({1, 2, 3}, {4, 5, 6}),                true);
TEST (char, [2][d(MAX)], Init ({1, 2, 3, 4}, {5, 6, 7, 8}),          true);

TEST (char, [d(MAX)][2], Init ({1}),                                 true);
TEST (char, [d(MAX)][2], Init ({1, 2}),                              true);
TEST (char, [d(MAX)][2], Init ({1}, {2}),                            true);
TEST (char, [d(MAX)][2], Init ({1, 2}, {3, 4}),                      true);
TEST (char, [d(MAX)][2], Init ({1, 2}, {3, 4}, {5, 6}),              true);
TEST (char, [d(MAX)][2], Init ({1, 2}, {3, 4}, {5, 6}, {7, 8}),      true);

TEST (char, [d(MAX)][d(MAX)], Init ({1}),                            true);
TEST (char, [d(MAX)][d(MAX)], Init ({1, 2}),                         true);
TEST (char, [d(MAX)][d(MAX)], Init ({1}, {2}),                       true);
TEST (char, [d(MAX)][d(MAX)], Init ({1, 2}, {3, 4}),                 true);
TEST (char, [d(MAX)][d(MAX)], Init ({1, 2}, {3, 4}, {5, 6}),         true);
TEST (char, [d(MAX)][d(MAX)], Init ({1, 2}, {3, 4}, {5, 6}, {7, 8}), true);

int main ()
{
  if (fail)
    __builtin_abort ();
}
