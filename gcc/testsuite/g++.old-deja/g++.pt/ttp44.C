// { dg-do assemble  }

template < class T, template < class > class E1, template < class > class E2 >
class Add {
public:
  Add(const E1<T>& e1, const E2<T>& e2) {}
};

template < class T >
struct Id {
  template < template < class > class E >
  Add < T, ::Id, E > operator+(const E<T>& e) const {
    return Add < T, ::Id, E >(*this, e);
  }
};

template struct Id<double>;
