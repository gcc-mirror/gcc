// { dg-do run  }
// Bug: g++ generates an error trying to generate the first foo<int>, when
// it should silently fail and go on to the next one.

template<class T, typename U> class A { };

template<class T> void
foo(const A<T,typename T::N>&);

template<typename T>
class B { };

template<typename T> void
foo(B<T> const &) { }

int
main(void)
{
  B<int> sa;

  foo<int> (sa);
}
