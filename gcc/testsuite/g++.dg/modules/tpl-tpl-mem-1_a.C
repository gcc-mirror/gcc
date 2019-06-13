// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-bmi foo }

template <typename T>
class outer 
{
public:
  template <typename U>
  struct inner 
  {
    typedef outer<U> other;
  };

  using type = T;
};

template class outer<int>;
