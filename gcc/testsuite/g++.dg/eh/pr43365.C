extern "C" void abort();

class Counter
{
public:
  static int count;
  ~Counter() { count += 1; }
};

int Counter::count = 0;

void func()
{
  Counter c;

  try {
    throw 1;
  }
  catch (const int&) {
    return;
  }
}

int main()
{
  func();
  if (Counter::count != 1)
    abort();
  return 0;
}
