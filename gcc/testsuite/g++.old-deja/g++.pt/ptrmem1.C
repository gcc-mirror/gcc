// Build don't run:

class foo
{
public:
  template<class T>
  T bar() {}
};

int
main()
{
  foo f;
  
  int (foo::*s)() = &foo::template bar<int>;
}
