// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// Perform access checking to parameter and return type of 
// function template correctly when the template is friend.

template <class T> class O {
  struct I { I (int); };

  template <class T_>
  friend typename O<T_>::I f ();
};

template <class T_>
typename O<T_>::I f () { return 1; }

struct X {
    void g() { f<int>(); }
};
