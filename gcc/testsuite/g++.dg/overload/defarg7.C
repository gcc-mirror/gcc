struct A
{
  A(const char *);
  explicit A(const int *);
};

void f (A a = 0);

int main()
{
  f();
}
