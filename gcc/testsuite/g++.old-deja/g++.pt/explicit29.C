// Build don't run:
// GROUPS passed templates
template <class T>
int foo(T) { return 0; }

int foo(int);

int main()
{
  return foo<int>(3);
}

