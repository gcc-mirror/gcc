// { dg-do compile { target c++11 } }

void f(int i);

int main()
{
  f({42.0});			// { dg-error "narrowing" }
  return {1.0};			// { dg-error "narrowing" }
}
