// { dg-do compile }

// Perform access checking to parameter and return type of 
// function template correctly when only specialization is friend.

template <class T>
typename T::Inner
foo () {
  return typename T::Inner();
}

class Outer {
  private:
    struct Inner {};

    friend Outer::Inner foo<Outer> ();
};

void f() {
  foo<Outer>();
}
