// { dg-do compile { target c++11 } }

// Test conversion to bool

#define assert_true(b) do { char c[2 * bool(b) - 1]; } while(0)

void fun()
{
  assert_true(nullptr ? false : true);
  decltype(nullptr) mynull = 0;
  assert_true(mynull ? false : true);
}
