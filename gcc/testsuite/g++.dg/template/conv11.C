int i;
struct A
{
  template <class T> operator T&() { return i; } // { dg-message "candidate" }
};

int main()
{
  A().operator int();		// { dg-error "operator int" }
}
