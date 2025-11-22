// PR c++/122699
// { dg-additional-options "-fmodules" }
// { dg-module-cmi X }

export module X;
import M;
ns::inner::S<int> s;
