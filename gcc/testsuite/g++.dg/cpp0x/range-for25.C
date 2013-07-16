// PR c++/57243
// { dg-require-effective-target c++11 }

struct snarf
{
  template <class T>
  void get() {}
};

template <class T>
struct container
{
  snarf * begin() { return nullptr; }
  snarf * end() { return nullptr; }
};

template <class T>
void foo()
{
  container<int> arr;

  for( auto i : arr )
    i.get<int>();
}

int main()
{
  return 0;
}

