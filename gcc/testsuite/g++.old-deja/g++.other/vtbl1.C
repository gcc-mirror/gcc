// Build don't run:
// Origin: DJ Delorie <dj@delorie.com>

class X {
public:
  int i;
  X() { i = 0; }
  ~X();
  virtual int foo() { return i; }
  virtual void bar();
};

int main()
{
}
