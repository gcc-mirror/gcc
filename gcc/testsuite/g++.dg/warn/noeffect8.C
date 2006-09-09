// PR c++/26696, 28996

struct A
{
  static void f() {}
}; 

int main() 
{
  A a; 
  a.f;    			// { dg-warning "not call" }
  A().f;  			// { dg-warning "not call" }
}
