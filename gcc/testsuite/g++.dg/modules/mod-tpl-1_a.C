// { dg-module-do run }

export module Frob;
// { dg-module-bmi "Frob" }

export template <typename T>
T twice (T x)
{
  return x * 2;
}

