// { dg-do assemble  }
// Make sure we can cast to a templated type, that requires a conversion by
// constructor, from a derived type to a base type.

// prms-id: 3524

template <class T>
struct ccPair {
    ccPair () { }
};

template <class T>
struct ccO : ccPair<T> {
  ccO () { }
};

void foo ()
{
  ccO<float> r;
  (ccPair<float>)r;
}
