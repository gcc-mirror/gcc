// PR c++/38392
// { dg-do link }

void Function();

int main()
{
  Function();
}

template <typename T>
struct Test
{
  friend void Function() { }
};

template class Test<int>;
