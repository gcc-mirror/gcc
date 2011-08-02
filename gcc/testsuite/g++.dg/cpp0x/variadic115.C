// PR c++/49593
// { dg-options -std=c++0x }

template<typename... T> void f(T...) { }

template<typename... Args>
static void
g(Args&&... args)
{
  f( static_cast<Args>(args)... );
  f( (Args)args... );
  f( Args(args)... );
  f( Args{args}... );
}

int main()
{
  g(1, '2', 3.0);
}
