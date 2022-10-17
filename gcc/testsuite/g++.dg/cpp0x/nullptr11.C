// { dg-do compile { target c++11 } }

// Test relational operators

#define assert_true(b) do { char c[2 * bool(b) - 1]; } while(0)
#define assert_false(b) do { char c[1 - 2 * bool(b)]; } while(0)

void fun()
{
  assert_true(nullptr == nullptr);
  assert_false(nullptr != nullptr);

  decltype(nullptr) mynull = 0;

  assert_true(mynull == nullptr);
  assert_false(mynull != nullptr);

  assert_true(nullptr == mynull);
  assert_false(nullptr != mynull);

  assert_true(mynull == mynull);
  assert_false(mynull != mynull);
}
