// Build don't link: 
// GROUPS passed old-abort
class First {
public:
  First(const First& a);
};

class Second {
  int i;
  First f;
public:
  ~Second() {}
  Second func();
};

void foo()
{
  extern Second x;
  x = x.func();
}
