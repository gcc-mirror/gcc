// { dg-lto-do link }
// { dg-lto-options { { -flto -g } } }

typedef int T;
void foo(void) {}
int main()
{
  foo();
  using ::T;
}
