// PR c++/83825
// { dg-do compile }

template <int A>
class A {};	// { dg-error "shadows template parameter" }

template <int I>
class B
{ 
  void foo () { A <I> a; }
};

template void B <0>::foo ();
