// PR c++/96862
// { dg-do compile { target c++17 } }
// { dg-additional-options "-frounding-math" }

constexpr double a = 0x1.0p+100 + 0x1.0p-100;
const double b = 0x1.0p+100 + 0x1.0p-100;
const double &&c = 0x1.0p+100 + 0x1.0p-100;
static_assert (0x1.0p+100 + 0x1.0p-100 == 0x1.0p+100, "");

void
foo ()
{
  constexpr double d = 0x1.0p+100 + 0x1.0p-100;
  const double e = 0x1.0p+100 + 0x1.0p-100;
  const double &&f = 0x1.0p+100 + 0x1.0p-100;
  static_assert (0x1.0p+100 + 0x1.0p-100 == 0x1.0p+100, "");
}

const double &g = a;
const double &h = b;
