// PR c++/21581
// Test for anonymous namespace default hidden visibility

// { dg-require-visibility "" }
// { dg-final { scan-hidden "_ZN.*1fEv" } }

namespace
{
  int f() { }
}
