// { dg-do compile { target c++11 } }

void swallow(...) {}
template<int... Is>
void foo() {
  int t = 0;
  swallow(
   ([&t]{return 0;}(), Is)...
  );
}

int main()
{
  foo<1, 2>();
  return 0;
}
