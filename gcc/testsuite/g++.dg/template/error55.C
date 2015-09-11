// Check that template template parameters get printed properly in error
// messages.

template <template <class A> class B>
struct Y
{
  B<5> y;  // { dg-error "for 'template<class A> class B'" }
};
