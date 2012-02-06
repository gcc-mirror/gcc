// PR c++/52088

struct S
{
  template <typename T>
  operator T *() { return 0; }
};

int main()
{
  S s;
  delete s;		       // { dg-error "ambiguous|template|pointer" }
}
