// { dg-do compile { target c++14 } }
// { dg-final { scan-assembler "_Z1fIiERDaRKT_S1_" } }

template <class T>
auto& f(const T& t, T u) { return t; }

int main()
{
  int i;
  f(i,i);
}
