// { dg-options "-std=gnu++0x" }
template<class X, class Y, class Z> X f(Y,Z); 
template<class... Args> void f2();
void g() 
{ 
  f<int,const char*,double>("aa",3.0); 
  f<int,const char*>("aa",3.0); // Z is deduced to be double 
  f<int>("aa",3.0); // Y is deduced to be char*, and 
                    // Z is deduced to be double 
  f("aa",3.0); // { dg-error "no matching" }
  f2<char, short, int, long>(); // okay
} 
