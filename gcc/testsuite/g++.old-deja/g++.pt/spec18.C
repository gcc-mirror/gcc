// Build don't link:

template<class A, class B>
void foo(const A& a, const B& b)
{
}

template<class A, class B>
void foo(const A& a, const int& b)
{
}

template<class A*, class B>
void foo(const A*& a, const B& b)
{
}

template<>
void foo(const int&, const double&)
{
}


int
main()
{
  foo("98239", 23);
  foo(232, 1.022);
}

