// { dg-require-effective-target c++11 }

int main()
{
  int x;
  auto f = [x]{ };
  f.__x.foo;			// { dg-error "<lambda\\(\\)>::<x capture>" }
}
