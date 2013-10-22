// { dg-options "-std=c++11" }
int main(void)
{
  static_assert(alignof(int) == __alignof(int), "alignof(int) does not equal __alignof(int)");
}
