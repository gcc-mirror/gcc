// { dg-do compile { target c++11 } }
template<class X, class Y, class Z> X f(Y,Z); // { dg-message "note" }
template<class... Args> void f2();
void g() 
{ 
  f<int,const char*,double>("aa",3.0); 
  f<int,const char*>("aa",3.0); // Z is deduced to be double 
  f<int>("aa",3.0); // Y is deduced to be char*, and 
                    // Z is deduced to be double 
  f("aa",3.0); // { dg-error "no matching" }
  // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 10 }
  f2<char, short, int, long>(); // okay
} 
