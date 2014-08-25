// N3648: capture init example from paper
// { dg-do run { target c++14 } }

int x = 4;
auto y = [&r = x, x = x+1]()->int {
  r += 2;
  return x+2;
}();  // Updates ::x to 6, and initializes y to 7.

int main()
{
  if (x != 6 || y != 7) __builtin_abort();
}
