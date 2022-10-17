struct D  { int i;  int pad alignas(16); };
struct B : virtual D
{
  int j =84;
  int k =84;
};

struct C: B { };

int main()
{
  C c;
  if (c.j != 84 || c.k != 84)
    __builtin_abort();
}
