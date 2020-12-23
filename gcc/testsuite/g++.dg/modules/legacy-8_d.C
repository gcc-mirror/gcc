// { dg-additional-options "-fmodules-ts" }
export module foo;
// { dg-module-cmi foo }

import "legacy-8_a.H";
import "legacy-8_b.H";

export inline int Sqr (int a)
{
  return sqr (a);
}

export inline float Sqr (float a)
{
  return sqr (a);
}
