// { dg-do compile { target c++11 } }

template <int a>
void f (const char (&)[a]) { }
void f (int) { }
template <class...a>
void
g ()
{
  f ({2u});
}
