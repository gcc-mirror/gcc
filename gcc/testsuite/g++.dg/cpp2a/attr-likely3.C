// { dg-do compile { target c++20 } }

[[likely]] void f() { }		// { dg-warning "function" }

int main()
{
  f();
}
