// { dg-options "-Wuseless-cast" }

template<typename T>
  void tmpl_f1(T& t)
  {
    (int)(t);
    static_cast<int>(t);
    reinterpret_cast<int>(t);

    (int*)(&t);
    const_cast<int*>(&t);
    static_cast<int*>(&t);
    reinterpret_cast<int*>(&t);

    (int&)(t);
    const_cast<int&>(t);
    static_cast<int&>(t);
    reinterpret_cast<int&>(t);
  }

template<typename T>
  void tmpl_f2(T t)
  {
    (int&)(t);
    const_cast<int&>(t);
    static_cast<int&>(t);
    reinterpret_cast<int&>(t);
  }

struct A { };

template<typename T>
  void tmpl_f3(T& t)
  {
    (A)(t);
    static_cast<A>(t);

    (A*)(&t);
    const_cast<A*>(&t);
    static_cast<A*>(&t);
    reinterpret_cast<A*>(&t);
    dynamic_cast<A*>(&t);

    (A&)(t);
    const_cast<A&>(t);
    static_cast<A&>(t);
    reinterpret_cast<A&>(t);
    dynamic_cast<A&>(t);
  }

template<typename T>
  void tmpl_f4(T t)
  {
    (A&)(t);
    const_cast<A&>(t);
    static_cast<A&>(t);
    reinterpret_cast<A&>(t);
    dynamic_cast<A&>(t);
  }

A prvalue();

void f()
{
  int n;

  (int)(n);
  static_cast<int>(n);
  reinterpret_cast<int>(n);

  (int*)(&n);                  // { dg-warning "3:useless cast" }
  const_cast<int*>(&n);        // { dg-warning "3:useless cast" }
  static_cast<int*>(&n);       // { dg-warning "3:useless cast" }
  reinterpret_cast<int*>(&n);  // { dg-warning "3:useless cast" }

  int& m = n;

  (int&)(m);                   // { dg-warning "3:useless cast" }
  const_cast<int&>(m);         // { dg-warning "3:useless cast" }
  static_cast<int&>(m);        // { dg-warning "3:useless cast" }
  reinterpret_cast<int&>(m);   // { dg-warning "3:useless cast" }

  tmpl_f1(m);

  (int&)(n);                   // { dg-warning "3:useless cast" }
  const_cast<int&>(n);         // { dg-warning "3:useless cast" }
  static_cast<int&>(n);        // { dg-warning "3:useless cast" }
  reinterpret_cast<int&>(n);   // { dg-warning "3:useless cast" }

  tmpl_f2(n);

#ifdef __GXX_EXPERIMENTAL_CXX0X__
  (int&&)(42);
  static_cast<int&&>(42);

  (A&&)(prvalue());
  const_cast<A&&>(prvalue());
  static_cast<A&&>(prvalue());
#endif

  A a;

  (A)(a);
  static_cast<A>(a);

  (A*)(&a);                   // { dg-warning "3:useless cast" }
  const_cast<A*>(&a);         // { dg-warning "3:useless cast" }
  static_cast<A*>(&a);        // { dg-warning "3:useless cast" }
  reinterpret_cast<A*>(&a);   // { dg-warning "3:useless cast" }
  dynamic_cast<A*>(&a);       // { dg-warning "3:useless cast" }

  A& b = a;

  (A&)(b);                    // { dg-warning "3:useless cast" }
  const_cast<A&>(b);          // { dg-warning "3:useless cast" }
  static_cast<A&>(b);         // { dg-warning "3:useless cast" }     
  static_cast<A&>(b);         // { dg-warning "3:useless cast" }
  dynamic_cast<A&>(b);        // { dg-warning "3:useless cast" }

  tmpl_f3(b);

  (A&)(a);                    // { dg-warning "3:useless cast" } 
  const_cast<A&>(a);          // { dg-warning "3:useless cast" }
  static_cast<A&>(a);         // { dg-warning "3:useless cast" }
  reinterpret_cast<A&>(a);    // { dg-warning "3:useless cast" }
  dynamic_cast<A&>(a);        // { dg-warning "3:useless cast" }

  tmpl_f4(a);
}
