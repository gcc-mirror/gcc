// PR c++/60417
// { dg-options -pedantic }

struct A { explicit A(int = 0); };

int main()
{
  A a[1] = { };			// { dg-warning "explicit" "" { target c++11 } }
}
