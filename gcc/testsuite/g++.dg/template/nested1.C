/* PR c++/4633 */
/* { dg-do compile } */

// The 'class X' inside the template used to escape (somehow),
// so that the typedef claimed that it was redefining X.

template <typename T> struct S ;

template <> struct S<float>
{
  template <class, class> struct R;
  template <class X> struct R<X, X> { };

  typedef int X;
};
