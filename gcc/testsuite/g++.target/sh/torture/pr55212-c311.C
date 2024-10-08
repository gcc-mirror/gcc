/* { dg-additional-options "-mlra -fpic" }  */
/* { dg-do compile }  */


typedef signed int int32_t;
typedef signed long long int int64_t;
static constexpr int32_t SK_MaxS32 = 214748364;;
static constexpr int32_t SK_MinS32 = -SK_MaxS32;

extern double fabs (double __x) noexcept (true) __attribute__ ((__const__));

namespace std __attribute__ ((__visibility__ ("default")))
{
using ::fabs;

template<typename _Tp> [[__nodiscard__]] constexpr inline const _Tp& min(const _Tp& __a, const _Tp& __b)
{
   if (__b < __a) return __b;
   return __a;
}

template<typename _Tp> [[__nodiscard__]] constexpr inline const _Tp& max(const _Tp& __a, const _Tp& __b)
{
  return __a;
}

}


static constexpr int32_t Sk64_pin_to_s32(int64_t x)
{
  return x < SK_MinS32 ? SK_MinS32 : (x > SK_MaxS32 ? SK_MaxS32 : (int32_t)x);
}

static constexpr int32_t Sk32_sat_sub(int32_t a, int32_t b)
{
  return Sk64_pin_to_s32((int64_t)a - (int64_t)b);
};

struct SkPoint
{
  float fX;
  float fY;
};

typedef float SkScalar;
static inline bool SkScalarNearlyEqual(SkScalar x, SkScalar y, SkScalar tolerance = (1.0f / (1 << 12)))
{
  return std::fabs(x-y) <= tolerance;
}

class SkCubicMap
{
public:
  SkCubicMap(SkPoint p1, SkPoint p2);
private:
  enum Type
  {
    kLine_Type, kCubeRoot_Type, kSolver_Type,
  };

  Type fType;
};

SkCubicMap::SkCubicMap(SkPoint p1, SkPoint p2)
{
  p1.fX = std::min(std::max(p1.fX, 0.0f), 1.0f);
  p2.fX = std::min(std::max(p2.fX, 0.0f), 1.0f);
  if (SkScalarNearlyEqual(p1.fX, p1.fY) && SkScalarNearlyEqual(p2.fX, p2.fY))
  {
    fType = kLine_Type;
  }
}
