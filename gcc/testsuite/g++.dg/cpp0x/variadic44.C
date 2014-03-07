// { dg-do compile { target c++11 } }
template<typename... Args>
int f(const Args&...);

template<typename T> void g(T) { }

void h()
{
  g(&f<int, float>);
}

// { dg-final { scan-assembler "_Z1gIPFiRKiRKfEEvT_"} }
