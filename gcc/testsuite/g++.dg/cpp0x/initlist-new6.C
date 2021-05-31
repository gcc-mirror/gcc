// PR c++/105265
// { dg-do run { target c++11 } }

int c;

class Block
{
public:
  Block(int n) : data{new char[n]}, size{n}
  {
    ++c;
  }

  ~Block()
  {
    --c;
    delete[] data;
  }

private:
  char* data;
  int size;
};

struct Cargo
{
  Block const& block;
};

int main()
{
  {
    Cargo* c = new Cargo{{4000}};
    delete c;
  }
  if (c != 0)
    __builtin_abort ();
  return 0;
}
