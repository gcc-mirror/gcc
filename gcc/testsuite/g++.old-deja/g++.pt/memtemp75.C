// Build don't link:

void
print(const int& i)
{
}

template<class A>
class bar
{
public:	
  template<void (*B)(const A& a)>
  void doit(const A& a)
    {
      B(a);
    }
};


int
main()
{
  bar<int>	b;
  b.template doit<print>(2);
}
