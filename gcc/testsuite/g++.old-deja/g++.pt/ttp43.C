// { dg-do assemble  }

template < class T, template <class> class E1, template <class> class E2 >
struct Add {
  Add(const E1<T>& e1, const E2<T>& e2) {}
};


template < class T, template <class> class E1, template <class> class E2 >
struct Mul {
  Mul(const E1<T>& e1, const E2<T>& e2) {}
};


template < class T >
struct Lit {
  Lit(const T& t) {}
};


template < class T >
struct Id {
  Add < T, ::Id, Lit > operator+(const T& t) const {
    return Add < T, ::Id, Lit >(*this, Lit<T>(t));
  }

  Mul < T, ::Id, Lit > operator*(const T& t) const {
    return Mul < T, ::Id, Lit >(*this, Lit<T>(t));
  }
};
