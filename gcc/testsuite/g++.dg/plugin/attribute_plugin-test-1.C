// { dg-warning "Callback to register attributes" "" { target *-*-* } 0 }

void normal_func (char c, char c2);
void normal_func (char __attribute__((user("param"))) c, char);
void normal_func (char c, char __attribute__((user("param"))) c2) 
{
} // { dg-warning "attribute 'user' on param 'c' of function normal_func" }
// { dg-warning "attribute 'user' on param 'c2' of function normal_func" "" { target *-*-* } .-1 }

class Foo {
  void method (char __attribute__((user("param"))) c);
};

void Foo::method(char c) 
{
} // { dg-warning "attribute 'user' on param 'c' of function method" }
