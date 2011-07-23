// { dg-options "-std=c++0x -pedantic-errors" }

void f(int i);

int main()
{
  f({42.0});			// { dg-error "narrowing" }
  return {1.0};			// { dg-error "narrowing" }
}
