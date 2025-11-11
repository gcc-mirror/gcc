// PR c++/122551
// { dg-additional-options "-fmodules" }
// { dg-module-cmi X }

export module X;
import M;

export S<int> test() {
  return {};
}
