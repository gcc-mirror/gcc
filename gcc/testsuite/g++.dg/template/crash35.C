// PR c++/20463
// { dg-do compile }

template <typename T> struct C; // { dg-error "declaration" }

template <typename T> void C<T>::f() // { dg-error "invalid|template" }
{
  const foo bar; // { dg-error "name a type" }
}
