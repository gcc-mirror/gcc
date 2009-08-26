struct C
{
  operator int();
};

struct D
{
  operator int();
};

int main()
{
  C c; D d;
  true ? c : d;
}
