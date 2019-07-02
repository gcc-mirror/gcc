// { dg-additional-options -fmodules-ts }
// { dg-module-do run }

export module node;
// { dg-module-cmi node }

export template <typename T> void assert (T t)
{
  static_assert (sizeof (T) == sizeof (int), "whoops");
}

export class other 
{
public:
  other () :f (5) {}
  void o () { }
  int f;
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

export inline bool ptrmemdata (other const &obj)
{
  int other::*ptr = &other::f;

  return (obj.*ptr) == 5;
}

export template<typename T> int ptrmemfn (T const &obj)
{
  int (T::*ptr) () const = &T::frob;
  return (obj.*ptr) ();
}
