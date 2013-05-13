// PR c++/57068

enum Enums {
  Enum1 = 0x00000000,
  Enum2 = 0x00000001
};

class Flags {
public:
  Flags() : i(0) {}
  Flags(int i): i(i) {}
  Flags operator&(Enums f) { return Flags(Enums(i & f)); }

  operator bool() { return i; }
private:
  int i;
};

Flags windowState()
{
  return Flags();
}

int main()
{
  if (bool(windowState() & Enum1) == true)
    return 1;
  return 0;
}
