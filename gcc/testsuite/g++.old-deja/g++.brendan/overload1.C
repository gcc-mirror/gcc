// Build don't link: 
// GROUPS passed overloading
class Foo
{
public:
  int f (void);
};

class Bar : public Foo
{
public:
      int f (int); // ERROR - candidates are
};

int main ()
{
  Bar b;

  b.f ();// ERROR - 
  b.f (10);
}
