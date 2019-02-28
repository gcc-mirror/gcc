// { dg-additional-options "-fmodules-ts" }
// This sequence is for errors

export module One;
// { dg-module-bmi "One" }

namespace Bob 
{
  struct X;
  export struct Y {
    unsigned a;
    unsigned b;
  };
}

export void copy (Bob::Y *, const Bob::Y *);
