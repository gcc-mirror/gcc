// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/17395
// { dg-do "compile" }

template<int> struct X { };

void fu(int a, X<sizeof(a)>) { }

template<class T>
void bhar(T a, X<sizeof(a)>) { }

int
main()
{
  int x;
  X<sizeof(int)> y;
  fu(x, y);
  bhar(x, y);
}
