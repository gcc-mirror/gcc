// Use a small template instantiation depth to speed up testing 
// { dg-options "-ftemplate-depth-5" }
// { dg-do compile }

// Origin: rullo.pat@tiscalinet.it
//	   Nathanael Nerode <neroden@gcc.gnu.org>
//	   Wolfgang Bangerth <bangerth@dealii.org>

// PR c++/6749: Infinite loop generating vtable.

template <class T> struct inner {};

template <class T> struct parent {
  virtual void f()
    { parent<inner<T> > p; };		// { dg-error "instantiation depth" }
};

template struct parent<int>;
