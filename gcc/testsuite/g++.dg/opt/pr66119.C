/* PR66119 - MOVE_RATIO is not constant in a compiler run, so Scalar
   Reduction of Aggregates must ask the back-end more than once what
   the value of MOVE_RATIO now is.  */

/* { dg-do compile  { target { { i?86-*-* x86_64-*-* } && c++11 } }  }  */
/* { dg-options "-O3 -mavx -fdump-tree-sra -march=slm -mtune=slm -fno-early-inlining" } */

#include <immintrin.h>

class MyAVX
{
  __m256d data;
public:
  MyAVX () = default;
  MyAVX (const MyAVX &) = default;
  MyAVX (__m256d _data) : data(_data) { ; }

  MyAVX & operator= (const MyAVX &) = default;

  operator __m256d () const { return data; }
  MyAVX operator+ (MyAVX s2) { return data+s2.data; }
};

template <typename T> class AVX_trait { ; };

template <> class AVX_trait<double> {
public:
  typedef __m256d TSIMD;
};


template <typename T>
class MyTSIMD
{
  typename AVX_trait<T>::TSIMD data;

public:
  MyTSIMD () = default;
  MyTSIMD (const MyTSIMD &) = default;
  // MyTSIMD (const MyTSIMD & s2) : data(s2.data) { ; }
  MyTSIMD (typename AVX_trait<T>::TSIMD _data) : data(_data) { ; }

  operator typename AVX_trait<T>::TSIMD() const { return data; }
  MyTSIMD operator+ (MyTSIMD s2) { return data+s2.data; }
};

// using MyVec = MyAVX;
using MyVec = MyTSIMD<double>;

class Vec2
{
  MyVec a, b;
public:
  Vec2 (MyVec aa, MyVec ab) : a(aa), b(ab) { ; }
  Vec2 operator+ (Vec2 v2) { return Vec2(a+v2.a, b+v2.b); }
};

inline __attribute__ ((__always_inline__))
Vec2 ComputeSomething (Vec2 a, Vec2 b)
{
  return a+b;
}

Vec2 TestFunction (Vec2 a, Vec2 b)
{
  return ComputeSomething (a,b);
}

/* { dg-final { scan-tree-dump "Created a replacement for b" "sra" } } */
