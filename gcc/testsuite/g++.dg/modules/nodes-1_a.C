// { dg-additional-options -fmodules-ts }
// { dg-module-do run }

export module node;
// { dg-module-bmi node }

export template <typename T> void assert (T t)
{
  static_assert (sizeof (T) == sizeof (int), "whoops");
}

export class other 
{
public:
  void o () { }
};

export template<typename T> class baselink : T, other
{
public:
  int Frob ()
  {
    o ();
    return this->T::frob ();
  }
};

export template <typename T> bool trait ()
{
  return __has_nothrow_assign(T);
}
