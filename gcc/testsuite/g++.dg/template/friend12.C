// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR 9030.  Perform access checking to parameter and return type of 
// function template correctly when the template is friend.

template <class T> class Outer {
  private:
    struct Inner {};

    template <class T_>
    friend typename Outer<T_>::Inner foo ();
};

template <class T>
typename Outer<T>::Inner
foo () {
  return typename Outer<T>::Inner();
}

void f() {
  foo<int>();
}
