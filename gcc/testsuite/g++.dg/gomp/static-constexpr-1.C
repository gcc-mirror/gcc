// { dg-do compile }
// { dg-require-effective-target c++11 }

/* Test that static constexpr members do not interfere with offloading.  */
struct rec
{
  static constexpr int x = 1;
  int y, z;
};

void foo (rec& r)
{
  #pragma omp target map(r)
  {
    r.y = r.y = r.x;
  }
}
