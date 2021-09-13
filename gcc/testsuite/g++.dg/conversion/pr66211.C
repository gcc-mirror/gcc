// PR c++/66211
// { dg-do compile }

void f(int&){}

int main()
{
  int x = 0;
  double y = 1;
  f(1 > 0 ? x : y); // { dg-error "cannot bind non-const lvalue reference of type .int&. to a value of type .double" }
}
