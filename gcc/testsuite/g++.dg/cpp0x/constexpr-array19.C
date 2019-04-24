// PR c++/89833
// Test to verify that constant array elements initialized to zero
// evaluate to zero regardless of the form of their initilizer,
// and irrespective whether it's explicit or implicit.

// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

static const char all_zero[1024] = { };

namespace test_int
{
constexpr int a[][3] = { { 0, 0 }, { 0 }, { } };

static_assert (sizeof a == sizeof (int) * 3 * 3);

static_assert (   a[0][0] == 0 && a[0][1] == 0 && a[0][2] == 0
               && a[1][0] == 0 && a[1][1] == 0 && a[1][2] == 0
               && a[2][0] == 0 && a[2][1] == 0 && a[2][2] == 0);

constexpr int b[3][3] = { { 0, 0 }, { 0 } };

static_assert (sizeof b == sizeof (int) * 3 * 3);

static_assert (   b[0][0] == 0 && b[0][1] == 0 && b[0][2] == 0
               && b[1][0] == 0 && b[1][1] == 0 && b[1][2] == 0
               && b[2][0] == 0 && b[2][1] == 0 && b[2][2] == 0);

constexpr int c[3][3] = { { } };

static_assert (sizeof c == sizeof (int) * 3 * 3);

static_assert (   c[0][0] == 0 && c[0][1] == 0 && c[0][2] == 0
               && c[1][0] == 0 && c[1][1] == 0 && c[1][2] == 0
               && c[2][0] == 0 && c[2][1] == 0 && c[2][2] == 0);

}

namespace test_char
{
constexpr char a[][3] = { { 0, 0 }, { 0 }, { } };

static_assert (sizeof a == sizeof (char) * 3 * 3);

static_assert (   a[0][0] == 0 && a[0][1] == 0 && a[0][2] == 0
               && a[1][0] == 0 && a[1][1] == 0 && a[1][2] == 0
               && a[2][0] == 0 && a[2][1] == 0 && a[2][2] == 0);

constexpr char b[3][3] = { { 0, 0 }, { 0 } };

static_assert (sizeof b == sizeof (char) * 3 * 3);

static_assert (   b[0][0] == 0 && b[0][1] == 0 && b[0][2] == 0
               && b[1][0] == 0 && b[1][1] == 0 && b[1][2] == 0
               && b[2][0] == 0 && b[2][1] == 0 && b[2][2] == 0);

constexpr char c[3][3] = { { } };

static_assert (sizeof c == sizeof (char) * 3 * 3);

static_assert (   c[0][0] == 0 && c[0][1] == 0 && c[0][2] == 0
               && c[1][0] == 0 && c[1][1] == 0 && c[1][2] == 0
               && c[2][0] == 0 && c[2][1] == 0 && c[2][2] == 0);
}

namespace test_string
{
constexpr char a[][3] = { "\0", "", { } };

static_assert (sizeof a == sizeof (char) * 3 * 3);

static_assert (   a[0][0] == 0 && a[0][1] == 0 && a[0][2] == 0
               && a[1][0] == 0 && a[1][1] == 0 && a[1][2] == 0
               && a[2][0] == 0 && a[2][1] == 0 && a[2][2] == 0);

constexpr char b[3][3] = { "\0", "" };

static_assert (sizeof b == sizeof (char) * 3 * 3);

static_assert (   b[0][0] == 0 && b[0][1] == 0 && b[0][2] == 0
               && b[1][0] == 0 && b[1][1] == 0 && b[1][2] == 0
               && b[2][0] == 0 && b[2][1] == 0 && b[2][2] == 0);

constexpr char c[3][3] = { };

static_assert (sizeof c == sizeof (char) * 3 * 3);

static_assert (   c[0][0] == 0 && c[0][1] == 0 && c[0][2] == 0
               && c[1][0] == 0 && c[1][1] == 0 && c[1][2] == 0
               && c[2][0] == 0 && c[2][1] == 0 && c[2][2] == 0);
}

namespace test_string_member
{
struct B { struct A { char a[5]; } a[2]; };

constexpr B b[3] =
  {
   /* [0] */
   {
    /* a = */
    {
     /* a[0] */ { { 0, 0, 0, 0, 0 } },
     /* a[1] */ { { 0, 0  } }
    }
   },
   /* [1] */
   {
    /* a */
    {
     /* a[0] */ { "\0\0\0\0" },
     /* a[0] */ { "" }
    }
   },
  };

static_assert (   b[0].a[0].a[0] == 0
	       && b[0].a[0].a[1] == 0
	       && b[0].a[0].a[2] == 0
	       && b[0].a[0].a[3] == 0
	       && b[0].a[0].a[4] == 0
	       && b[0].a[1].a[0] == 0
	       && b[0].a[1].a[1] == 0
	       && b[0].a[1].a[2] == 0
	       && b[0].a[1].a[3] == 0
	       && b[0].a[1].a[4] == 0
	       && b[1].a[0].a[0] == 0
	       && b[1].a[0].a[1] == 0
	       && b[1].a[0].a[2] == 0
	       && b[1].a[0].a[3] == 0
	       && b[1].a[0].a[4] == 0
	       && b[2].a[0].a[0] == 0
	       && b[2].a[0].a[1] == 0
	       && b[2].a[0].a[2] == 0
	       && b[2].a[0].a[3] == 0
	       && b[2].a[0].a[4] == 0);
}
