// { dg-options "-std=c++0x" }

template<class U, class... T>
void f()			// { dg-message "note" }
{
  f<T...>(); // { dg-error "no matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 6 }
}

template<>
void f() { } // { dg-error "template-id" }

int main()
{
  f<char>();
}
