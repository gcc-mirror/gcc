// { dg-do compile { target c++14 } }
// { dg-additional-options "-Wno-return-type" }

constexpr int f (int i)
{
  if (i == -1) return 0;
}

constexpr int i = f(42);	// { dg-error "flows off the end|in .constexpr. expansion of " }
