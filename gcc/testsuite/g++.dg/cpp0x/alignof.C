// { dg-options "-std=c++0x" }
int main(void)
{
  static_assert(alignof(int) == __alignof(int), "alignof(int) does not equal __alignof(int)");
}
