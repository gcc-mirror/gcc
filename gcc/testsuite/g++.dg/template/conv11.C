int i;
struct A
{
  template <class T> operator T&() { return i; } // { dg-message "note" }
};

int main()
{
  A().operator int();		// { dg-error "operator int" }
  // { dg-message "(candidate|mismatched types)" "candidate note" { target *-*-* } 9 }
}
