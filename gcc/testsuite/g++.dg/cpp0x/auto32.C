// { dg-do compile { target c++11 } }

// { dg-final { scan-assembler "_Z1fIiEDTnw_Dapifp_EET_" } }
template <class T> auto f(T t) -> decltype (new auto(t));

int main()
{
  f(1);
}
