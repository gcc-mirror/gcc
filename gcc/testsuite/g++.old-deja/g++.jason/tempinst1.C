// { dg-do link  }
// { dg-options "-g" }
// Bug: g++ fails to instantiate operator<<.

struct ostream {
  ostream& operator<< (const char *) { return *this; };
};

template <class T> class foo;

template <class T> ostream& operator<< (ostream& ios, foo<T>&obj) {return ios;}

template <class T> class foo {
  friend ostream& operator<<<>(ostream&, foo<T>&);
};

int main()
{
  ostream cout;
  foo<int> foo_obj;
  cout << foo_obj; // causes linker error
  return 0;
} 
