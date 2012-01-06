// { dg-options "-fabi-version=0" }

template <unsigned int> struct helper {};
// { dg-final { scan-assembler "_Z6check1IiEvP6helperIXszscT_Li1EEE" } }
template <class T> void check1( helper<sizeof(static_cast<T>(1))> * ) { }
// { dg-final { scan-assembler "_Z6check2IiXadL_Z1iEEEvP6helperIXszccPT_T0_EE" } }
template <class T, T* p> void check2( helper<sizeof(const_cast<T*>(p))> * ) { }
// { dg-final { scan-assembler "_Z6check3IiEvP6helperIXszrcPT_Li0EEE" } }
template <class T> void check3( helper<sizeof(reinterpret_cast<T*>(0))> * ) { }
// { dg-final { scan-assembler "_Z6check4I1AXadL_Z1aEEEvP6helperIXszdcPT_T0_EE" } }
template <class T, T* p> void check4( helper<sizeof(dynamic_cast<T*>(p))> * ) { }

struct A{} a;
int i;
int main()
{
  check1<int>(0);
  check2<int,&i>(0);
  check3<int>(0);
  check4<A,&a>(0);
}
