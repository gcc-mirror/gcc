// { dg-do compile { target c++11 } }

int main()
{
  [](int a = 1) { return a; }(); // { dg-error "default argument" "" { target { c++11_only } } }
}
