// PR c++/50870
// { dg-options -std=c++0x }

template <class V>
  struct impl
  {
    template <class T> static T create();
  };

template <class T, class U, class V, class
      = decltype(impl<V>::template create<T>()
             -> impl<V>::template create<U>())>
struct tester { };

tester<impl<float>*, int, float> ti;
