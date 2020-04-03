// PR c++/82521
// { dg-additional-options "-Wtype-limits" }

template <typename T>
const char * g(const unsigned char value)
{
  return value == -1 ? "-1" : "no"; // { dg-warning "always false" }
}

int main()
{
  g<int>(-1);
}
