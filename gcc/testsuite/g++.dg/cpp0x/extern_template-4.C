// PR c++/85470
// { dg-do compile { target c++11 } }

template <class T>
struct StaticObject
{
    static T& create()
    {
      static T t;
      return t;
    }

    static T & instance;
};

template <class T> T & StaticObject<T>::instance = StaticObject<T>::create();

extern template class StaticObject<int>;

void test()
{
    StaticObject<int>::instance;
}
