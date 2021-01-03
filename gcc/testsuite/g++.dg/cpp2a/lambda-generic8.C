// PR c++/97839
// { dg-do compile { target c++20 } }
// Test that a lambda with <template-param-list> doesn't require
// a lambda-declarator.

int main()
{
  []<typename T>{}.operator()<int>();
}
