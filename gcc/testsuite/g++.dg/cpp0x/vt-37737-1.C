// { dg-do compile { target c++11 } }
// { dg-prune-output "note" }

void f() { }

template<class U, class... T>
void f(){ f<T...>(); } // { dg-error "no matching" }

int main()
{ 
  f<char>();
}
