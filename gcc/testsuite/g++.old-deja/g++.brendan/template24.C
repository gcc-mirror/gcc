// GROUPS passed templates
extern "C" void printf (char *, ...);

template <class F>
class Temp
{
  F  func_;
public:
  Temp (F f) :func_(f) {}
};

int func (int h = 1, int z = 2) { return h+z; }

int main ()
{
  Temp<int(*)(int, int)> temp (func);

  printf ("PASS\n");
  return 0;
}
