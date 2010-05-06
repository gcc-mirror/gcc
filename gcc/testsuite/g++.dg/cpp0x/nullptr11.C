// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test relational operators

#define assert_true(b) do { char c[2 * bool(b) - 1]; } while(0)
#define assert_false(b) do { char c[1 - 2 * bool(b)]; } while(0)

void fun()
{
  assert_true(nullptr == nullptr);
  assert_false(nullptr != nullptr);
  assert_false(nullptr < nullptr);
  assert_false(nullptr > nullptr);
  assert_true(nullptr <= nullptr);
  assert_true(nullptr >= nullptr);
}
