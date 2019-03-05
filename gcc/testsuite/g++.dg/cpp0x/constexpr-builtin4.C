// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-psabi" }

typedef int v4si __attribute__((vector_size (4 * sizeof (int))));
typedef float v4sf __attribute__((vector_size (4 * sizeof (float))));
constexpr v4sf a = __builtin_convertvector (v4si { 1, 2, -3, -4 }, v4sf);

constexpr v4sf
foo (v4si x)
{
  return __builtin_convertvector (x, v4sf);
}

constexpr v4sf b = foo (v4si { 3, 4, -1, -2 });

static_assert (a[0] == 1.0f && a[1] == 2.0f && a[2] == -3.0f && a[3] == -4.0f, "");
static_assert (b[0] == 3.0f && b[1] == 4.0f && b[2] == -1.0f && b[3] == -2.0f, "");
