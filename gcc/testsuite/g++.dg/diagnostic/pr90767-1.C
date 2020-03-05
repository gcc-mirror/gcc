// PR c++/90767
// { dg-do compile }

struct X {
  int n;
  void foo ();	// { dg-message "initializing argument 'this'" }

  template<typename T>
  operator T () const
    {
      if (n == 0)
	foo ();	// { dg-error "cannot convert 'const X\\*' to 'X\\*'" }
      return n;
    }
};
