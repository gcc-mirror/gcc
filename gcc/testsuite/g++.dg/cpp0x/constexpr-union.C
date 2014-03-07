// Test that we don't have to deal with type punning
// DR 1188 says this is ill-formed
// { dg-do compile { target c++11 } }

union U
{
  float f;
  unsigned char ca[sizeof(float)];
};

constexpr U u = { 1.0 };
constexpr float f = u.f;
constexpr unsigned char c = u.ca[0]; // { dg-error "U::ca" }

constexpr double d = 1.0;
constexpr unsigned char c2 = (unsigned char&)d; // { dg-error "char. glvalue" }
