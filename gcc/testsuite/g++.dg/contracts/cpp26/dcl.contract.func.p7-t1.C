// N5008:
// dcl.contract.func/p7
// If the predicate of a postcondition assertion of a function f odr-uses (6.3) a non-reference parameter of f,
// that parameter and the corresponding parameter on all declarations of f shall have const type
// These tests instantiate templates with a non const type.
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts" }
#include <type_traits>
struct NTClass {
    NTClass(){};
    ~NTClass(){};
    int x = 1;
};

template <typename... ARGS>
bool check(ARGS... args){ return true;}

namespace checkDeclaration{

template <class T>
void ft1 (T i, T &j, T *k, const T * l, T * const m)
  post ( check (i)) // { dg-error "used in a postcondition must be const" }
  post ( check (j))
  post ( check (k)) // { dg-error "used in a postcondition must be const" }
  post ( check (l)) // { dg-error "used in a postcondition must be const" }
  post ( check (m)){};

template void ft1<int>(int, int&, int*, int const*, int* const);
// { dg-error "value parameter 'i'" "" { target *-*-* } 21 }
// { dg-error "value parameter 'k'" "" { target *-*-* } 21 }
// { dg-error "value parameter 'l'" "" { target *-*-* } 21 }


struct PostCond {

  template <class T>
  PostCond  (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m)){};

  template <class T>
  void ft1 (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m)){};

};

template
PostCond::PostCond<int> (int i,  int &j, int *k, const int * l,  int * const m);
// { dg-error "value parameter 'i'" "" { target *-*-* } 37 }
// { dg-error "value parameter 'k'" "" { target *-*-* } 37 }
// { dg-error "value parameter 'l'" "" { target *-*-* } 37 }

template
void PostCond::ft1<NTClass>(NTClass, NTClass&, NTClass*, NTClass const*, NTClass* const);
// { dg-error "value parameter 'i'" "" { target *-*-* } 45 }
// { dg-error "value parameter 'k'" "" { target *-*-* } 45 }
// { dg-error "value parameter 'l'" "" { target *-*-* } 45 }


template <typename T>
struct PostCondT
{

  PostCondT (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i))
    post ( check (j))
    post ( check (k))
    post ( check (l))
    post ( check (m)){};

  template <class U>
  void f (U i, U &j, U *k, const U * l, U * const m)
    post ( check (i))
    post ( check (j))
    post ( check (k))
    post ( check (l))
    post ( check (m)){};
};


template
PostCondT<int>::PostCondT (int i,  int &j, int *k, const int * l,  int * const m);
// { dg-error "value parameter 'i'" "" { target *-*-* } 71 }
// { dg-error "value parameter 'k'" "" { target *-*-* } 71 }
// { dg-error "value parameter 'l'" "" { target *-*-* } 71 }

template
void PostCondT<int>::f<NTClass>(NTClass, NTClass&, NTClass*, NTClass const*, NTClass* const);
// { dg-error "value parameter 'i'" "" { target *-*-* } 79 }
// { dg-error "value parameter 'k'" "" { target *-*-* } 79 }
// { dg-error "value parameter 'l'" "" { target *-*-* } 79 }

template <typename T>
int f2(const T i[10])
post(r : r == i[0]){ return 1;};

template
int f2<int>(const int i[10]);
// { dg-error "used in a postcondition must be const" "" { target *-*-* } 101  }

// P3520
template <typename T>
void f4(std::add_const_t<T> t) post(t > 0){}; //ok
template <typename T>
void f5(T t) post (t > 0){};

void test2(){
  f4<int>(1);
  f5<int>(1); // { dg-error "used in a postcondition must be const" "" { target *-*-* } 112  }
}
}


namespace checkNonOdr{

template <class T>
void f (T i, T *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j))){};

template
void f<int>(int, int*);

struct PostCond {
  template <class T>
  void f (T i, T *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j))){};
};

template
void PostCond::f<int>(int, int*);


template <typename T>
struct PostCondT
{
  void f1 (T i, T *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j))){};

  template <class U>
  void f2 (U i, U *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j))){};
};

template
void PostCondT<int>::f1(int, int*);

template
void PostCondT<int>::f2<NTClass>(NTClass, NTClass*);

}
