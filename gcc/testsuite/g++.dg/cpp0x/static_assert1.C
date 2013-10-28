// { dg-options "-std=c++11" }
void foo()
{
  static_assert(1, "okay");
  static_assert (0 == 1, "zero is never equal to one"); // { dg-error "never equal" }
}

class X {
  static_assert(1, "okay");
  static_assert (0 == 1, "zero is never equal to one"); // { dg-error "never equal" }
};

static_assert(1, "okay");
static_assert (0 == 1, "zero is never equal to one"); // { dg-error "never equal" }
