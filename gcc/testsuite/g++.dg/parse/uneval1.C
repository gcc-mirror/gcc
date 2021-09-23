// PR c++/93314

struct S {
  int m;
  static int f() {
    return sizeof(char[m]);	// { dg-error "S::m" }
  }
};

int main()
{
  return S().f()
    + sizeof(char[S::m]);	// { dg-error "S::m" }
}
