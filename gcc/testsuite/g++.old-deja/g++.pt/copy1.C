int i = 0;

template <class T>
class F 
{
public:
  F() {}
  
  template <class T2> F(F<T2>) 
    {
      i = 1;
    }      
};


F<int>
foo()
{
  F<int> f1;
  F<int> f2(f1);
  return f1;
}

int
main()
{
  return i;
}

