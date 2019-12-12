// { dg-do compile { target c++2a } }

[[likely]] void f() { }		// { dg-warning "function" }

int main()
{
  f();
}
