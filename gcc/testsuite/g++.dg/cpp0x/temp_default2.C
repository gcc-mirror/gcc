// { dg-options "-std=c++0x" }

template <class T, class U = double> 
void f(T t = 0, U u = 0); 	// { dg-message "note" }

void g() 
{ 
  f(1, 'c'); // f<int,char>(1,'c') 
  f(1); // f<int,double>(1,0) 
  f(); // { dg-error "no matching function" }
  // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 10 }
  f<int>(); // f<int,double>(0,0) 
  f<int,char>(); // f<int,char>(0,0) 
} 
