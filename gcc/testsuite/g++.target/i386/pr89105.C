// PR c++/89105
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=12 -Wabi=11" }

namespace {
  template<typename F>
    void run(F f, int i)	// { dg-bogus "parameter passing ABI changes in -fabi-version=12" }
    {
      f(i);
    }
}

void f()
{
  run([](int) { }, 1);		// { dg-bogus "parameter passing ABI changes in -fabi-version=12" }
}
