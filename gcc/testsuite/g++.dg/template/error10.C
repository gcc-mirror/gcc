// { dg-do compile }
// Origin: <tilps at hotmail dot com>
// c++/9154: poor error message for ">>" vs "> >" in template argument list


/*
 * Test that the error message is issued properly
 */
template <class T>
class A {};

A<A<int>> blah;  // { dg-error "should be '> >' within" }
A<int>> blah2; // { dg-error "spurious '>>'" }


/*
 * Test that a few valid constructs containing a ">>" token in a
 * template argument list are handled correctly.
 */
template <int N>
void B(void) {}

int Btest()
{
  B<256 >> 4>();
}

template <int N = 123>>4>
struct C {};

template <int>      struct D {};
template <typename> struct E {};

E<D< 1>>2 > > E1;

const int x = 0;
E<D< 1>>x > > E2;

template <int> struct F {
  typedef int I;
};

template <typename T = F< 1>>2 >::I>
struct G {};

/*
 * In this special case, a valid type-id (H() is a function type) is followed
 * by '>>', but the argument should still be parsed as an expression, which
 * will then be rejected as non-constant expression.
 */
struct H
{
  int operator >>(int);
};

template <int V> struct L {};
L<H() >> 5> l;  // { dg-error "" "non-constant" }


/*
 * This case used to not emit the nice error message because of a typo
 *  in the code.
 */
template <void (*)(void)>
struct K {};

void KFunc(void);

A<K<&KFunc>> k1;  // { dg-error "" }
K<&KFunc>> k2; // { dg-error "" }
