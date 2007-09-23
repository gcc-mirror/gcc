// An intermediate version of the fix for c++/19407 broke this example.

struct A
{
  typedef struct { int i; } S;
};

template <class T>
struct B: public A
{
  template <class U>
  static S f ();
};

template struct B<int>;
