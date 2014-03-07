// { dg-do compile { target c++11 } }

template<class U, class... T>
void f()			// { dg-message "note" }
{
  f<T...>(); // { dg-error "no matching" }
  // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 6 }
}

template<>
void f() { } // { dg-error "template-id" }

int main()
{
  f<char>();
}
