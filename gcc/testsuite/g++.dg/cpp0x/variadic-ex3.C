// { dg-options "-std=gnu++0x" }
template<class X, class Y, class... Z> X f(Y); // { dg-message "note" }
void g() 
{ 
  int i = f<int>(5.6);
  int j = f(5.6);         // { dg-error "no matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 6 }
  f<void>(f<int, bool>);
  f<void>(f<int>);        // { dg-error "no matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 9 }
} 
