// { dg-do compile }
// { dg-options "-std=c++23 -fcontracts -fcontracts-nonattr " }

struct NTClass {
  //TODO, make non trivial when https://github.com/NinaRanns/gcc/issues/21 is solved
//  NTClass(){};
//  ~NTClass(){};
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
int f (T i, T &j, T *k, const T * l, T * const m)
  post ( check (i)) // { dg-error "used in a postcondition must be const" }
  post ( check (j))
  post ( check (k)) // { dg-error "used in a postcondition must be const" }
  post ( check (l)) // { dg-error "used in a postcondition must be const" }
  post ( check (m));

struct PostCond {

  PostCond (NTClass i, NTClass &j, NTClass *k, const NTClass * l, NTClass * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m));

  template <class T>
  PostCond  (T i, T &j, T *k, const T * l, T * const m)
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
  int f (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m));
};

template <typename T>
struct PostCondT
{

  PostCondT (NTClass i, NTClass &j, NTClass *k, const NTClass * l, NTClass * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m));

  template <class U>
  PostCondT (U i, U &j, U *k, const U * l, U * const m)
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

  int f (T i, T &j, T *k, const T * l, T * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m));

  template <class U>
  int f (U i, U &j, U *k, const U * l, U * const m)
    post ( check (i)) // { dg-error "used in a postcondition must be const" }
    post ( check (j))
    post ( check (k)) // { dg-error "used in a postcondition must be const" }
    post ( check (l)) // { dg-error "used in a postcondition must be const" }
    post ( check (m));
};


// P29000R14  [dcl.contract.func]/p7
int f(const int i[10])
post(r : r == i[0]); // { dg-error "used in a postcondition must be const" }

template <typename T>
int f(const T i[10])
post(r : r == i[0]); // { dg-error "used in a postcondition must be const" }

}
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
  f (T i); // { dg-error "used in a postcondition must be const" }

  void
  PostCond::f (NTClass i){} // { dg-error "used in a postcondition must be const" }

  template<class T>
  void
  PostCond::f (T i){} // { dg-error "used in a postcondition must be const" }

  template<typename T>
  void PostCondT<T>::f (NTClass i){} // { dg-error "used in a postcondition must be const" }

  template<typename T>
  void PostCondT<T>::f (T i){} // { dg-error "used in a postcondition must be const" }

  template<typename T>
  template<typename U>
  void PostCondT<T>::f (U i){} // { dg-error "used in a postcondition must be const" }

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
