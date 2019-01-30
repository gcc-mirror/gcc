// { dg-additional-options -fmodules-ts }
export module inter;
// { dg-module-bmi inter }

import worker;

namespace hidden {
int fn (int x)
{
  return -x;
}
}

export template <typename T>
int TPL (T &t)
{
  return fn (t);
}
