// PR c++/28048

template<typename T> struct Boom;

template<typename T, bool D = Boom<T>::Internal::Value> // <--ICE
    struct Foo
    {
    };

template<typename T> struct Boom
{
    struct Internal
    {
      static const bool Value = false;
    };
};
