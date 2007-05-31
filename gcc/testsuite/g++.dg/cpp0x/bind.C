// { dg-options "--std=c++0x" }
struct S{};
void f(S&&);

int main()
{
  f(S());
}
