// { dg-do assemble  }
// GROUPS passed friends
class A
{
private:
  A () {}

friend struct B;
};

class B
{
public:
  A a;
};

B b;

int main () {}
