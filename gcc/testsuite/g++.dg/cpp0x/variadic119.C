// PR c++/51046
// { dg-do compile { target c++11 } }

template<int... IS>
void f()
{
  for (int i : IS);		// { dg-error "not expanded" }
}

int main()
{
  f<0, 1, 2>();
}
