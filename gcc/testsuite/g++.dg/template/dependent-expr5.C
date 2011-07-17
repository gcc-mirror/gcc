// { dg-do compile }

// Copyright 2005 Free Software Foundation
// contributed by Alexandre Oliva <aoliva@redhat.com>
// inspired in the failure reported in Red Hat bugzilla #168260.

template<class F> void bind(F f) {} // { dg-message "note" }

template<class F> void bindm(F f) {} // { dg-message "note" }
template<class F, class T> void bindm(F (T::*f)(void)) {} // { dg-message "note" }

template<class F> void bindn(F f) {}
template<class F, class T> void bindn(F (*f)(T)) {}

template<class F> void bindb(F f) {}
template<class F, class T> void bindb(F (*f)(T)) {} // { dg-message "note" }
template<class F, class T> void bindb(F (T::*f)(void)) {} // { dg-message "note" }

struct foo {
  static int baist;
  int bait;			// { dg-error "non-static data member" }
  void barf ();
  static void barf (int);

  struct bar {
    static int baikst;
    int baikt;
    void bark ();
    static void bark (int);

    bar() {
      bind (&baist);
      bind (&foo::baist);
      bind (&bait); // { dg-error "from this location" }
      bind (&foo::bait);

      bind (&baikst);
      bind (&bar::baikst);
      bind (&baikt); // ok, this->baikt
      bind (&bar::baikt);

      bind (&barf); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 42 }
      bind (&foo::barf); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 44 }

      bindm (&barf); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 47 }
      bindm (&foo::barf);

      bindn (&barf);
      bindn (&foo::barf);

      bindb (&barf);
      bindb (&foo::barf); // { dg-error "ambiguous" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 55 }

      bind (&bark); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 58 }
      bind (&bar::bark); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 60 }

      bindm (&bark); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 63 }
      bindm (&bar::bark);

      bindn (&bark);
      bindn (&bar::bark);

      bindb (&bark);
      bindb (&bar::bark); // { dg-error "ambiguous" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 71 }
    }
  };

  template <typename T>
  struct barT {
    static int baikst;
    int baikt;
    void bark ();
    static void bark (int);

    barT() {
      bind (&baist);
      bind (&foo::baist);
      bind (&bait); // { dg-error "from this location" }
      bind (&foo::bait);

      bind (&baikst);
      bind (&barT::baikst);
      bind (&baikt); // ok, this->baikt
      bind (&barT::baikt);

      bind (&barf); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 94 }
      bind (&foo::barf); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 96 }

      bindm (&barf); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 99 }
      bindm (&foo::barf);

      bindn (&barf);
      bindn (&foo::barf);

      bindb (&barf);
      bindb (&foo::barf); // { dg-error "ambiguous" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 107 }

      bind (&bark); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 110 }
      bind (&barT::bark); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 112 }

      bindm (&bark); // { dg-error "no matching function" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 115 }
      bindm (&barT::bark);

      bindn (&bark);
      bindn (&barT::bark);

      bindb (&bark);
      bindb (&barT::bark); // { dg-error "ambiguous" }
      // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 123 }
    }
  };

  bar bard;
  barT<void> bart;
} bad;
