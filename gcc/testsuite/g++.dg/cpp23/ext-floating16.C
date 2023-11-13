// P1467R9 - Extended floating-point types and standard names.
// { dg-do compile { target c++23 } }
// { dg-options "-pedantic-errors -Wno-narrowing" }
// { dg-add-options float16 }
// { dg-add-options float32 }
// { dg-add-options float64 }
// { dg-add-options float128 }

#include "ext-floating.h"

#ifdef __STRICT_ANSI__
#undef __SIZEOF_FLOAT128__
#endif

using namespace std;

#ifdef __STDCPP_FLOAT16_T__
#ifdef __STDCPP_FLOAT32_T__
float16_t f16c = 1.0F32;		// { dg-bogus "converting to 'std::float16_t' \\\{aka '_Float16'\\\} from '_Float32' with greater conversion rank" "" { target { float16 && float32 } } }
#endif
#ifdef __STDCPP_FLOAT64_T__
float16_t f16e = 1.0F64;		// { dg-bogus "converting to 'std::float16_t' \\\{aka '_Float16'\\\} from '_Float64' with greater conversion rank" "" { target { float16 && float64 } } }
#endif
#ifdef __STDCPP_FLOAT128_T__
float16_t f16g = 1.0F128;		// { dg-bogus "converting to 'std::float16_t' \\\{aka '_Float16'\\\} from '_Float128' with greater conversion rank" "" { target { float16 && float128 } } }
#endif
#endif
#ifdef __STDCPP_FLOAT32_T__
#ifdef __STDCPP_FLOAT64_T__
float32_t f32e = 1.0F64;		// { dg-bogus "converting to 'std::float32_t' \\\{aka '_Float32'\\\} from '_Float64' with greater conversion rank" "" { target { float32 && float64 } } }
#endif
#ifdef __STDCPP_FLOAT128_T__
float32_t f32g = 1.0F128;		// { dg-bogus "converting to 'std::float32_t' \\\{aka '_Float32'\\\} from '_Float128' with greater conversion rank" "" { target { float32 && float128 } } }
#endif
#endif
#ifdef __STDCPP_FLOAT64_T__
#ifdef __STDCPP_FLOAT128_T__
float64_t f64g = 1.0F128;		// { dg-bogus "converting to 'std::float64_t' \\\{aka '_Float64'\\\} from '_Float128' with greater conversion rank" "" { target { float64 && float128 } } }
#endif
#endif
