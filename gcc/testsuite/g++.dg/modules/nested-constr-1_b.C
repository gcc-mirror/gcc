// { dg-additional-options "-std=c++2a -fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

#include "nested-constr-1.h"
import "nested-constr-1_a.H";

struct X 
{
  using type = int;
};

traits<char>::nested<X>::type b;

// { dg-final { scan-lang-dump-not {merge key \(new\)} module } }
