// Bug: g++ fails to instantiate operator<<.
// Build don't run:
// Special g++ Options: -g

struct ostream {
  ostream& operator<< (const char *) { return *this; };
};

template <class T> class foo;

template <class T> ostream& operator<< (ostream& ios, foo<T>&obj) { };

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
