// { dg-options "-std=gnu++0x" }
template<class X, class Y, class... Z> X f(Y); 
void g() 
{ 
  int i = f<int>(5.6);
  int j = f(5.6);         // { dg-error "no matching" }
  f<void>(f<int, bool>); 
  f<void>(f<int>);        // { dg-error "no matching" }
} 
