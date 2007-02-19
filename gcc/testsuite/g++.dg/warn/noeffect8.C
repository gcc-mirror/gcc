// PR c++/26696, 28996
// { dg-do compile }
// { dg-options "-Waddress" }

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
