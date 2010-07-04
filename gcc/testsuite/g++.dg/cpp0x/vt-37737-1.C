// { dg-options "-std=c++0x" }
// { dg-prune-output "note" }

void f() { }

template<class U, class... T>
void f(){ f<T...>(); } // { dg-error "no matching" }

int main()
{ 
  f<char>();
}
