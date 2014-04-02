// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=0" }

void* operator new (__SIZE_TYPE__, void *p) { return p; }
int i;

template <unsigned int> struct helper {};
// { dg-final { scan-assembler "_Z6check1IiEvP6helperIXsznw_T_EEE" } }
template <class T> void check1( helper<sizeof(new T)> * ) { }
// { dg-final { scan-assembler "_Z6check2IiEvP6helperIXszgsnw_T_piEEE" } }
template <class T> void check2( helper<sizeof(::new T())> * ) { }
// { dg-final { scan-assembler "_Z6check3IiEvP6helperIXsznwadL_Z1iE_T_piLi1EEEE" } }
template <class T> void check3( helper<sizeof(new (&i) T(1))> * ) { }
// { dg-final { scan-assembler "_Z7check3aIiEvP6helperIXsznw_T_ilLi1EEEE" } }
template <class T> void check3a( helper<sizeof(new T{1})> * ) { }
// { dg-final { scan-assembler "_Z6check4IiEvP6helperIXszna_A1_T_EEE" } }
template <class T> void check4( helper<sizeof(new T[1])> * ) { }
// { dg-final { scan-assembler "_Z6check5IiEvP6helperIXszna_A1_T_piEEE" } }
template <class T> void check5( helper<sizeof(new T[1]())> * ) { }
int main()
{
  check1<int>(0);
  check2<int>(0);
  check3<int>(0);
  check3a<int>(0);
  check4<int>(0);
  check5<int>(0);
}
