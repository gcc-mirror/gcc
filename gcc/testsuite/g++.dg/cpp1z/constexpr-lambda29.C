// Test that we don't make lambdas with goto/static implicitly constexpr
// when an explicitly constexpr function would be ill-formed.

// { dg-do compile { target c++17 } }

int main()
{
  constexpr int a = [] {
    return 42;
    goto label;
  label:
    return 142;
  }();				// { dg-error "" "" { target c++20_down } }

  constexpr int b = [] {
    return 42;
    static int i;
  }();				// { dg-error "" "" { target c++20_down } }
}
