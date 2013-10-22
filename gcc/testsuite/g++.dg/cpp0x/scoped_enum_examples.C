// { dg-do compile }
// { dg-options "-std=c++11" }
enum class Col { red, yellow, green };
                                
int x = Col::red; // { dg-error "cannot convert" }
Col y = Col::red;
           
void f()
{                     
  if (y) { } // { dg-error "could not convert" }
}

enum direction { left='l', right='r' };
void g() {
                                // OK
  direction d;
                                // OK
  d = left;
                                // OK
  d = direction::right;
}
enum class altitude { high='h', low='l' };
void h() {
  altitude a;
  a = high;                     // { dg-error "not declared in this scope" }
  a = altitude::low;
}
