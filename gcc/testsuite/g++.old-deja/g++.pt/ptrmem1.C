class foo
{
public:
  template<class T>
  T bar() { return 7; }
};

int
main()
{
  foo f;
  
  int (foo::*s)() = &foo::template bar<int>;
  if ((f.*s)() == 7)
    return 0;
  else 
    return 1;
}
