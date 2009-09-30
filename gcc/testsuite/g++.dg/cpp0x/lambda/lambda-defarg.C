// { dg-options "-std=c++0x -pedantic-errors" }

int main()
{
  [](int a = 1) { return a; }(); // { dg-error "" }
}
