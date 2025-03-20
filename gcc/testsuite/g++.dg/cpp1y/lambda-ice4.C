// PR c++/114292
// { dg-do "compile" { target c++14 } }
// { dg-additional-options "-Wno-vla" }

#define ASSERT_CAPTURE_NUMBER(Lambda, NumCaptures) \
  { \
    auto oneCapture = [&](auto) { int t[c]; }; \
    const auto sizeOneCapture = sizeof (oneCapture); \
    const auto expected = NumCaptures ? NumCaptures * sizeOneCapture : 1; \
    static_assert (sizeof (Lambda) == expected, ""); \
  }

template<int r, int c>
struct Want_a_Typedef { typedef int Type[r*c]; };
    
void foo (int c)
{
  constexpr int r = 4;

  // This used to ICE.
  auto ice_1 = [&](auto) { int t[c * r]; };
  ice_1 (0);
  ASSERT_CAPTURE_NUMBER (ice_1, 2);

  // Another ICE identified following a great question in the patch submission
  // mail thread.
  auto ice_2 = [&](auto) { typedef int MyT[c*r]; };
  ice_2 (0);
  ASSERT_CAPTURE_NUMBER (ice_2, 2);

  // All those worked already, but were not covered by any test - do it here.
  auto ok_0 = [&](auto) { typedef int MyT[c*r]; MyT t; };
  ok_0 (0);
  ASSERT_CAPTURE_NUMBER (ok_0, 2);

  auto ok_1 = [&](auto) { Want_a_Typedef<r, 42>::Type t; };
  ok_1 (0);
  ASSERT_CAPTURE_NUMBER (ok_1, 0);

  auto ok_2 = [&](auto) { int t[c]; };
  ok_2 (0);
  ASSERT_CAPTURE_NUMBER (ok_2, 1);

  auto ok_3 = [&](auto) { int n = r * c; int t[n]; };
  ok_3 (0);
  ASSERT_CAPTURE_NUMBER (ok_3, 2);

  auto ok_4 = [&](auto) { int t[r]; };
  ok_4 (0);
  ASSERT_CAPTURE_NUMBER (ok_4, 0);

  auto ok_5 = [&](auto) { int t[c * 4]; };
  ok_5 (0);
  ASSERT_CAPTURE_NUMBER (ok_5, 1);

  auto ok_6 = [&](auto) { int t[1]; };
  ok_6 (0);
  ASSERT_CAPTURE_NUMBER (ok_6, 0);

  auto ok_7 = [&](auto) { int t[c * r]; };
  ok_7 (0);
  ASSERT_CAPTURE_NUMBER (ok_7, 2);
}
