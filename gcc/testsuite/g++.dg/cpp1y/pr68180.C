// PR c++/68180
// { dg-do compile { target c++14 } }
// { dg-additional-options "-Wno-psabi" }

typedef float __attribute__( ( vector_size( 16 ) ) ) float32x4_t;
constexpr float32x4_t fill(float x) {
  float32x4_t v{0};
  constexpr auto vs = sizeof(v)/sizeof(v[0]);
  for (auto i=0U; i<vs; ++i) v[i]=i;
  return v+x;
}

float32x4_t foo(float32x4_t x) {
  constexpr float32x4_t v = fill(1.f); // { dg-error "not a constant||in .constexpr. expansion of " }
  return x+v;
}
