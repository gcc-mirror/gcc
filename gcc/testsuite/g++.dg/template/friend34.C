// { dg-do compile }

// Origin: mleone@pixar.com
//	   Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/9783: Forward declaration of class in template.

template <typename T>
struct C {
  void foo (struct X *);
};

struct X {};

template <typename T>
void C<T>::foo(struct X *) {}
