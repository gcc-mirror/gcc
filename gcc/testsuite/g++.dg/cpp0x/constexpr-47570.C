// PR c++/47570
// { dg-options -std=c++11 }

unsigned int constexpr one()
{ return 1; }

int constexpr one_B()
{ return 1; }

int main()
{
  // FAIL TO COMPILE:
  static bool constexpr SC_huh1 = ((unsigned int)one()) >= ((unsigned int)0);
  static bool constexpr SC_huh2 = one() >= ((unsigned int)0);
  static bool constexpr SC_huh3 = one() >= 0;

  // COMPILE OK:
  static bool constexpr SC_huh4 = ((one() == 0) || (one() > 0));
  static bool constexpr SC_huh5 = one() == 0;
  static bool constexpr SC_huh6 = one() > 0;
  static bool constexpr SC_huh7 = one_B() >= 0;
  static bool constexpr SC_huh8 = one() >= 1;

  return SC_huh3;
}
