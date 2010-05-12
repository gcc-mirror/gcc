int i;
struct A
{
  template <class T> operator T&() { return i; }
};

int main()
{
  A().operator int();		// { dg-error "operator int" }
}
