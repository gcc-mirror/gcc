// PR c++/78209
// { dg-do compile { target c++14 } }

int main()
{
  int &&i = 0;
  decltype(auto) j = i; // { dg-error "cannot bind rvalue reference" }
}
