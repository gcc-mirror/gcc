// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

export module Frob;
// { dg-module-cmi "Frob" }

export template <typename T>
T twice (T x)
{
  return x * 2;
}

