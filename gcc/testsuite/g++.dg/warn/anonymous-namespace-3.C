// Test for the warning of exposing types from an anonymous namespace
// { dg-do compile }
//
#include <memory>
#include "anonymous-namespace-3.h"

struct B { std::auto_ptr<A> p; };

#line 10 "foo.C"
struct C		   // { dg-warning "uses the anonymous namespace" }
{
  std::auto_ptr<A> p;
};

// { dg-prune-output "auto_ptr. is deprecated" }
