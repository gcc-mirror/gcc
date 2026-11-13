/* Test coverage of initializers that use RANGE_EXPR.  */
// { dg-do compile { target c++11 } }

struct s1 { int x; };
int s1::* const arr_of_s1[8] = { &s1::x };

bool
test_1 (int i)
{
  return arr_of_s1[i] == nullptr;
}

struct s2 { int v = 42; };
const s2 arr_of_s2[8] = {};

int
test_2 (int i)
{
  return arr_of_s2[i].v;
}
