// { dg-do assemble  }
// GROUPS passed old-abort
class foo {
private:
  char buffer[1024];
public:
  foo();
};

int main()
{
  static foo& a = *(new foo);
}
