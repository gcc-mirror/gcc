
// The anticipated decl for 'log' got retained, leading to confusion  */

extern double log (double) throw ();

namespace std
{
  using ::log;
  float log (float) throw ();
  long double log (long double) throw ();
}

void Foo (double x)
{
  std::log (x);
}
