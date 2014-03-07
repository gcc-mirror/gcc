// { dg-do compile { target c++11 } }

bool b;
// { dg-final { scan-assembler "_Z1fIiEDTquL_Z1bEfp_twLi42EET_" } }
template <class T> auto f (T t) -> decltype(b?t:throw 42) { return 0; }
// { dg-final { scan-assembler "_Z2f2IiEDTquL_Z1bEfp_trET_" } }
template <class T> auto f2 (T t) -> decltype(b?t:throw) { return 0; }

int main()
{
  f(0);
  f2(0);
}
