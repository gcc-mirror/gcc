// N5008:
// dcl.contract.func/p7
// If the predicate of a postcondition assertion of a function f odr-uses (6.3) a non-reference parameter of f,
// that parameter and the corresponding parameter on all declarations of f shall have const type
// These tests instantiate templates with a const type.
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
  post ( check (i))
  post ( check (j))
  post ( check (k)) // { dg-error "used in a postcondition must be const" }
  post ( check (l)) // { dg-error "used in a postcondition must be const" }
  post ( check (m)){};

template void ft1<const int>(const int, const int&, const int*, int const*, const int* const);
// { dg-error "value parameter 'k'" "" { target *-*-* } 21 }
// { dg-error "value parameter 'l'" "" { target *-*-* } 21 }


struct PostCond {

  template <class T>
  PostCond  (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i))
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m)){};

  template <class T>
  void ft1 (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i))
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m)){};

};

template
PostCond::PostCond<const int> (const int i, const int &j,const int *k, const int * l, const int * const m);
// { dg-error "value parameter 'k'" "" { target *-*-* } 36 }
// { dg-error "value parameter 'l'" "" { target *-*-* } 36 }

template
void PostCond::ft1<const NTClass>(const NTClass, const NTClass&, const NTClass*, NTClass const*, const NTClass* const);
// { dg-error "value parameter 'k'" "" { target *-*-* } 44 }
// { dg-error "value parameter 'l'" "" { target *-*-* } 44 }


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
PostCondT<const int>::PostCondT (const int i, const int &j, const int *k, const int * l, const int * const m);
// { dg-error "value parameter 'k'" "" { target *-*-* } 68 }
// { dg-error "value parameter 'l'" "" { target *-*-* } 68 }

template
void PostCondT<const int>::f<const NTClass>(const NTClass, const NTClass&, const NTClass*, NTClass const*, const NTClass* const);
// { dg-error "value parameter 'k'" "" { target *-*-* } 76 }
// { dg-error "value parameter 'l'" "" { target *-*-* } 76 }

template <typename T>
int f2(const T i[10])
post(r : r == i[0]){ return 1;};

template
int f2<const int>(const int i[10]);
// { dg-error "used in a postcondition must be const" "" { target *-*-* } 96  }

// P3520
template <typename T>
void f4(std::add_const_t<T> t) post(t > 0){}; //ok
template <typename T>
void f5(T t) post (t > 0){};

void test2(){
  int i[10];
  f4<const int>(1);
  f5<const int>(1);
}
}


namespace checkNonOdr{

template <class T>
void f (T i, T *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j))){};

template
void f<const int>(const int, const int*);

struct PostCond {
  template <class T>
  void f (T i, T *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j))){};
};

template
void PostCond::f<const int>(const int, const int*);


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
void PostCondT<const int>::f1(const int, const int*);

template
void PostCondT<const int>::f2<const NTClass>(const NTClass, const NTClass*);

}
