// Build don't link:
// excess errors test

// Here we declare ::S
typedef struct s1 *S;

struct s1
{
  int s;
};

struct A
{
  // Here we declare A::S
  typedef struct s1 *S;
};

template<class T, class U> class XX;

template<class T, class U>
class X
{
public:
  static T *do_something ();
  friend class T; // ERROR - `T' is a template parameter
  friend class XX<T, U>;
};

struct N
{
  // Here we declare N::S
  class S
  {
  };

  // Should use N::S and A::S.
  typedef X<S, A::S> X_S;

  void bug ();
};

void
N::bug ()
{
  // X_S is template class X<N::S, A::S>
  // `s' is N::S.
  S *s = X_S::do_something ();
}

