// PR c++/117397
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template <typename T> struct S;
import "dguide-5_b.H";
S<int> foo();
