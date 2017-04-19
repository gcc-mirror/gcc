// { dg-do compile { target c++11 } }
template<class X, class Y, class... Z> X f(Y); // { dg-message "note" }
void g() 
{ 
  int i = f<int>(5.6);
  int j = f(5.6);         // { dg-error "no matching" }
  // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } .-1 }
  f<void>(f<int, bool>);
  f<void>(f<int>);        // { dg-error "no matching" }
  // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } .-1 }
} 
