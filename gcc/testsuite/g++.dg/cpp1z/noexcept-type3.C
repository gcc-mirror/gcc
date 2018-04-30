// Test for overload resolution.
// { dg-options -std=c++17 }

void f(void (*)() noexcept) = delete;
void f(void (*)()) { }
void g() {}
void h() noexcept {}

int main()
{
  f(g);
  f(h);				// { dg-error "deleted" }
}
