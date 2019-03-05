// PR 87531 part 2.  dependent using decls + template decls.

template<typename T>
struct One
{
  One& operator=(T* p_)
  {
    return operator=<T>(p_); // Parse failed here
  }

  template<typename U>
  One& operator=(U* p_);
  
};


template<typename T>
struct Two : T
{
  using T::f;
  template<typename U> void f ();

  using T::operator T*;
  operator T * () const;
  
  int frob ()
  {
    return f<int> (1);
  }

  T *quux ()
  {
    return operator T * ();
  }

  T *quux () const
  {
    return operator T * ();
  }
};

struct Base 
{
  template <typename T> int f (T i) 
  {
    return i;
  }

  operator Base *() const;
};

void foo ()
{
  One<int> one;
  Two<Base> two;

  one = One<int> ();

  two.frob ();
  two.quux ();
  const_cast <const Two<Base> &> (two).quux ();
}

