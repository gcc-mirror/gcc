// { dg-do assemble  }
// GROUPS passed overloading
class Foo
{
public:
  int f (void);
};

class Bar : public Foo
{
public:
      int f (int); // { dg-error "" } candidates are
};

int main ()
{
  Bar b;

  b.f ();// { dg-error "" } 
  b.f (10);
}
