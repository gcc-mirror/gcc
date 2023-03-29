// It's an error to redeclare a name after using it in the class, but be
// lenient if it has the same meaning.

// { dg-options "" }

struct Lock { };
struct Traits
{
  Lock lock;
  typedef ::Lock Lock;		// { dg-warning -Wchanges-meaning }
};
struct Traits2
{
  Lock lock;
  typedef int Lock;		// { dg-error -Wchanges-meaning }
};
