// DR 2855, Undefined behavior in postfix increment
// { dg-do compile { target c++14 } }

using int8_t = signed char;

constexpr int
f ()
{
  int8_t x = 127;
  x++;
  int8_t z = 127;
  ++z;
  return x + z;
}

constexpr int i = f();
