// { dg-do compile }

// Origin: Jakub Jelinek <jakub@gcc.gnu.org>
//	   Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/19311: Non-dependent address to member as function argument.

template <class R, class T>          void foo (R (T::*x) ()); 
template <class R, class T, class C> void foo (R (T::*x) (C)); 
 
template<int> struct I { 
  int o (); 
  int o () const; 
}; 
 
template <int> void bar (void) { 
  foo <int, I<1> > (&I<1>::o); 
}
