// PR c++/97973

void (*foo[1])(const int &);
void (*foo2[1])(const double &);

template<typename>
void f ()
{
  (foo[0])(1.1);
  (foo2[0])(1);
}

void
g ()
{
  f<char> ();
}
