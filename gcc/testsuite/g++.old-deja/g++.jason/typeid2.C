// { dg-do run  }
// PRMS Id: 11596

#include <typeinfo>
extern "C" int printf (const char *, ...);

class Chicken
{
public:
  int eggs_per_day;
};

template <class Bird>
class Flock
{
public:
  Bird * flock_head;
  int    head_count;
  void print_self() {
    printf ("A flock of %d %ss\n", head_count, typeid (Bird).name ());
    printf ("A flock of %d %ss\n", head_count, typeid (*flock_head).name ());
  }
};

int main()
{
  Flock<Chicken> x;
  printf ("%s\n", typeid(x).name());
  x.head_count = 42;
  x.print_self();
}
