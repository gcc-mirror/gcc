// PR c++/23839

class C
{
  int i;
public:
  C(int j) : i(j) { }
  operator int() { return i; }
};

C f (register C x)
{
  return x + 31;
}
