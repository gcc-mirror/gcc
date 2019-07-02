// { dg-additional-options "-fmodules-ts" }
export module Const;
// { dg-module-cmi "Const" }

export constexpr int SQ (int b)
{
  return b * b;
}
