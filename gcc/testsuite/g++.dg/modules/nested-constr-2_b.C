// { dg-additional-options "-std=c++2a -fmodules-ts" }
export module bar;
// { dg-module-cmi bar }
import foo;

struct X 
{
  using type = int;
};

export traits<char>::nested<X>::type b;
