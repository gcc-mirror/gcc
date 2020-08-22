// DR 1512
// PR c++/87699
// { dg-do compile { target c++11 } }

template<class T, decltype((((T*) 0) < nullptr), true) = false> // { dg-error "ordered comparison" }
bool test(T*)
{
  return true;
}

int main()
{
  test((int*)(nullptr)); // { dg-error "no matching function" }
}
