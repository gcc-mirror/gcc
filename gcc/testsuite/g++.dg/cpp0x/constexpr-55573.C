// PR c++/55573
// { dg-do compile }
// { dg-options "-std=gnu++11" }
// Ignore warning on some powerpc-ibm-aix configurations.
// { dg-prune-output "non-standard ABI extension" }

template <typename T, int N>
struct ExtVecTraits {
  typedef T __attribute__((vector_size (N * sizeof (T)))) type;
};

template <typename T>
using Vec4 = typename ExtVecTraits<T,4>::type;

template <typename T>
struct Rot3
{
  typedef Vec4<T> Vec;
  Vec axis[3];
  constexpr Rot3 (Vec4<T> ix, Vec4<T> iy, Vec4<T> iz) : axis {ix, iy, iz} {}
};

typedef Vec4<float> Vec;
Rot3<float> r2 ((Vec) {0, 1, 0, 0}, (Vec){0, 0, 1, 0}, (Vec){1, 0, 0, 0});
