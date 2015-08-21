// { dg-do assemble  }
// GROUPS passed overloading
enum bar {};

void operator+ (int, int);// { dg-error "" } .*
void operator+ (bar&, int);

template <class T> void operator+ (int b, T& t) { (void) T::bogus; }
void operator+ (int, bar&);

template <class T> class foo
{
public:
  friend void operator+ <> (int, T&);
};

class baz;

class foo<int>;
class foo<baz>;
