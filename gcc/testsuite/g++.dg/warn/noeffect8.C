// PR c++/26696

struct A
{
  static void f() {}
}; 

int main() 
{
  A a; 
  a.f;    			// { dg-warning "not call" }
}
