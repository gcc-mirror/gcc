// { dg-do compile }

// PR c++/7804
// Floating point formatting in error and warning messages

extern "C" int printf(const char *, ...);

struct S
{
  static const float inf = 1.0f / 0.0f; // { dg-warning "1.0|initializ" }
  static const float nan = 0.0f / 0.0f; // { dg-warning "0.0|initializ" }
};

int main()
{
  printf("%f\n%f\n", S::inf, S::nan);
  return 0;
}
