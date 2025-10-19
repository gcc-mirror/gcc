// N5008:
// dcl.contract.func/p7
// If the predicate of a postcondition assertion of a function f odr-uses (6.3) a non-reference parameter of f,
// that parameter and the corresponding parameter on all declarations of f shall have const type
// These tests do not instantiate templates with diagnosable errors
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

int f (int i, int &j, int *k, const int * l, int * const m)
  post ( check (i)) // { dg-error "used in a postcondition must be const" }
  post ( check (j))
  post ( check (k)) // { dg-error "used in a postcondition must be const" }
  post ( check (l)) // { dg-error "used in a postcondition must be const" }
  post ( check (m));

int f (NTClass i, NTClass &j, NTClass *k, const NTClass * l, NTClass * const m)
  post ( check (i)) // { dg-error "used in a postcondition must be const" }
  post ( check (j))
  post ( check (k)) // { dg-error "used in a postcondition must be const" }
  post ( check (l)) // { dg-error "used in a postcondition must be const" }
  post ( check (m));

template <class T>
void ft1 (T i, T &j, T *k, const T * l, T * const m)
  post ( check (i))
  post ( check (j))
  post ( check (k))
  post ( check (l))
  post ( check (m)){};


struct PostCond {

  PostCond (NTClass i, NTClass &j, NTClass *k, const NTClass * l, NTClass * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m));

  template <class T>
  PostCond  (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i))
    post ( check (j))
    post ( check (k))
    post ( check (l))
    post ( check (m));

  int f (NTClass i, NTClass &j, NTClass *k, const NTClass * l, NTClass * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m));

  template <class T>
  int ft1 (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i))
    post ( check (j))
    post ( check (k))
    post ( check (l))
    post ( check (m));
};

/* Never instantiated to check errors without an instantiation.  */
template <typename T>
struct PostCondT
{

  PostCondT (NTClass i, NTClass &j, NTClass *k, const NTClass * l, NTClass * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m));

  PostCondT (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i))
    post ( check (j))
    post ( check (k))
    post ( check (l))
    post ( check (m));

  template <class U>
  PostCondT (U i, U &j, U *k, const U * l, U * const m)
    post ( check (i))
    post ( check (j))
    post ( check (k))
    post ( check (l))
    post ( check (m));

  int f (NTClass i, NTClass &j, NTClass *k, const NTClass * l, NTClass * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m));

  int f (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i))
    post ( check (j))
    post ( check (k))
    post ( check (l))
    post ( check (m));

  template <class U>
  int f (U i, U &j, U *k, const U * l, U * const m)
    post ( check (i))
    post ( check (j))
    post ( check (k))
    post ( check (l))
    post ( check (m));
};


// P29000R14  [dcl.contract.func]/p7
int f(const int i[10])
post(r : r == i[0]); // { dg-error "used in a postcondition must be const" }

template <typename T>
int f(const T i[10])
post(r : r == i[0]);

}

// P3520
using const_int_t = const int;
void f3(const_int_t i) post (i > 0);  //ok

template <typename T>
void f4(std::add_const_t<T> t) post(t > 0); //ok

template <typename T>
void f5(T t) post (t > 0);


namespace nonFirstDeclaration
{

  int f (const NTClass i)
    post ( check (i));

  template <class T>
  int f (const T i)
    post ( check (i));

  struct PostCond {
    void f (const NTClass i)
      post ( check (i));

    template <class T>
    void f (const T i)
      post ( check (i));
  };

  template <typename T>
  struct PostCondT
  {
    void f (const NTClass i)
      post ( check (i));

    void f (const T i)
      post ( check (i));

    template <class U>
    void f (const U i)
      post ( check (i));
  };

  int
  f (NTClass i); // { dg-error "used in a postcondition must be const" }

  template<class T>
  int
  f (T i);

  void
  PostCond::f (NTClass i){} // { dg-error "used in a postcondition must be const" }

  template<class T>
  void
  PostCond::f (T i){}

  template<typename T>
  void PostCondT<T>::f (NTClass i){} // { dg-error "used in a postcondition must be const" }

  template<typename T>
  void PostCondT<T>::f (T i){}

  template<typename T>
  template<typename U>
  void PostCondT<T>::f (U i){}

}

namespace nonFirstDeclarationNegative
{

  int f (const NTClass i)
    post ( check (true))
    pre ( check (i));

  template <class T>
  int f (const T i)
    post ( check (true))
    pre ( check (i));

  struct PostCond {
    void f (const NTClass i)
      post ( check (true))
      pre ( check (i));

    template <class T>
    void f (const T i)
      post ( check (true))
      pre ( check (i));
  };

  template <typename T>
  struct PostCondT
  {
    void f (const NTClass i)
      post ( check (true))
      pre ( check (i));

    void f (const T i)
      post ( check (true))
      pre ( check (i));

    template <class U>
    void f (const U i)
      post ( check (true))
      pre ( check (i));
  };

  int
  f (NTClass i);

  template<class T>
  int
  f (T i);

  void
  PostCond::f (NTClass i){}

  template<class T>
  void
  PostCond::f (T i){}

  template<typename T>
  void PostCondT<T>::f (NTClass i){}

  template<typename T>
  void PostCondT<T>::f (T i){}

  template<typename T>
  template<typename U>
  void PostCondT<T>::f (U i){}

}

namespace checkNonOdr{

int f (int i, int *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j)));


template <class T>
int f (T i, T *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j)));

struct PostCond {

  PostCond (int i, int *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j)));


  template <class T>
  PostCond  (T i, T *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j)));

  int f (int i, int *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j)));

  template <class T>
  int f (T i, T *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j)));
};

template <typename T>
struct PostCondT
{

  PostCondT (int i, int *j)
   post ( sizeof(i) )
   post ( noexcept(check (&j)));

  template <class U>
  PostCondT (U i, U *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j)));

  int f (int i, int *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j)));

  int f (T i, T *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j)));

  template <class U>
  int f (U i, U *j)
  post ( sizeof(i) )
  post ( noexcept(check (&j)));
};


}
