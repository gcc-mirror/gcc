// Test for explicit visibility taking precedence

// { dg-require-visibility "" }
// { dg-final { scan-not-hidden "_ZN1AIiE1fEv" } }

template <class T> struct A
{
  // This attribute takes precedence over...
  __attribute ((visibility ("default"))) void f ();
};

template <class T>
void A<T>::f ()
{ }

// ...this attribute.
template struct __attribute ((visibility ("hidden"))) A<int>;
