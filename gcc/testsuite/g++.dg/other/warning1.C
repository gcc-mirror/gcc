// { dg-do compile }

// PR c++/7804
// Floating point formatting in error and warning messages

extern "C" int printf(const char *, ...);

struct S
{
  static const float inf = 1.0f / 0.0f; // { dg-error "1.0|float|initializ" }
  static const float nan = 0.0f / 0.0f; // { dg-error "0.0|float|initializ" }
};

int main()
{
  // { dg-prune-output "not a member" }
  printf("%f\n%f\n", S::inf, S::nan);
  return 0;
}
