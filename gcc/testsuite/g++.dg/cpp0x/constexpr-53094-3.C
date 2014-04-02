// { dg-do compile { target c++11 } }
// { dg-options "" }
// Ignore warning on some powerpc-ibm-aix configurations.
// { dg-prune-output "non-standard ABI extension" }

typedef float __attribute__ ((vector_size (4 * sizeof (float)))) V4;

struct Rot3 {
  typedef float T;
  typedef V4 Vec;
  Vec axis[3];
  constexpr Rot3 (V4 ix, V4 iy, V4 iz) : axis {ix, iy, iz} {}

  constexpr Rot3(T xx, T xy, T xz, T yx, T yy, T yz, T zx, T zy, T zz) :
    Rot3((Vec) { xx, xy, xz, 0 },
	 (Vec) { yx, yy, yz, 0 },
	 (Vec) { zx, zy, zz, 0 }) {}

};

constexpr Rot3 r1( 0, 1 ,0, 0, 0, 1,  1, 0, 0);
