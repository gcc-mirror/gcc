// PR c++/50870
// { dg-do compile { target c++11 } }

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
